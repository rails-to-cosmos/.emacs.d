;;; ray-cluster.el --- Ray cluster views in a table-view -*- lexical-binding: t; -*-

;; Author: Dmitry Akatov <akatovda@gmail.com>

;;; Commentary:
;; Browse a Ray cluster (served by the ray-cluster-manager service) as
;; table-views.  Each command asks (completion, match required) which cluster,
;; then shows one resource:
;;
;;   M-x table-view-ray-actors      actors: id, class, name, state, restarts, pid, job
;;   M-x table-view-ray-jobs        jobs: id, status, entrypoint, submission, start/end
;;   M-x table-view-ray-nodes       nodes: role, cpu/mem/disk %, running/pending tasks
;;   M-x table-view-ray-tasks       cluster operations: op, status, created/finished
;;
;; In every view: `g' refreshes the row at point, `G' refreshes the whole view,
;; RET/C-c C-o/mouse-1 follows the Org link in id/job/node cells (into the Ray
;; dashboard at http://HEAD_IP:8265/#/…).  Point `table-view-ray-base-url' at
;; the manager.

;;; Code:

(require 'table-view)
(require 'cl-lib)
(require 'url)
(require 'json)

(defgroup table-view-ray nil
  "Ray cluster views in a table-view." :group 'table-view :prefix "table-view-ray-")

(defcustom table-view-ray-base-url "http://10.17.5.9:8000"
  "Base URL of the ray-cluster-manager service." :type 'string)

(defcustom table-view-ray-dashboard-port 8265
  "Ray dashboard port, used to build dashboard links." :type 'integer)

;;; HTTP + formatting helpers

(defun table-view-ray--get (path)
  "GET PATH from the manager and return the parsed JSON (alists and lists)."
  (let ((url (concat (string-trim-right table-view-ray-base-url "/") path)))
    (with-current-buffer (url-retrieve-synchronously url t t 20)
      (unwind-protect
          (progn
            (unless (and (boundp 'url-http-response-status)
                         (eql url-http-response-status 200))
              (error "GET %s failed: HTTP %s" url
                     (and (boundp 'url-http-response-status) url-http-response-status)))
            (goto-char (or (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                           (point-min)))
            (json-parse-buffer :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil))
        (kill-buffer)))))

(defun table-view-ray--head (cluster)
  "Return the head IP of CLUSTER (empty string when unknown)."
  (or (alist-get 'head_ip (table-view-ray--get (format "/api/clusters/%s/head" cluster))) ""))

(defun table-view-ray--ts (v)
  "Format epoch V (seconds or milliseconds) as a short local time, else \"\"."
  (if (and (numberp v) (> v 0))
      (format-time-string "%m-%d %H:%M" (seconds-to-time (if (> v 1e12) (/ v 1000.0) v)))
    ""))

(defun table-view-ray--trunc (s n)
  "S truncated to N characters with an ellipsis."
  (let ((s (or s "")))
    (if (> (length s) n) (concat (substring s 0 (- n 1)) "…") s)))

(defun table-view-ray--link (head-ip path desc)
  "Org link to the Ray dashboard at HEAD-IP under PATH, shown as DESC.
Returns plain DESC when HEAD-IP or DESC is empty."
  (if (and head-ip (not (string-empty-p head-ip)) desc (not (string-empty-p desc)))
      (format "[[http://%s:%d/#/%s][%s]]" head-ip table-view-ray-dashboard-port path desc)
    (or desc "")))

(defun table-view-ray--read-cluster ()
  "Prompt for a cluster name (completion, match required)."
  (completing-read "Ray cluster: "
                   (mapcar (lambda (c) (alist-get 'name c)) (table-view-ray--get "/api/clusters/"))
                   nil t))

(defun table-view-ray--refresh (buf rows-fn &optional id)
  "Refetch rows via ROWS-FN into BUF.
With ID, update only that row in place (dropping it when it is gone); else
replace the whole view.  No per-item endpoint exists, so refreshing one row
still fetches the list -- it just updates a single line and keeps point."
  (if (and id (not (equal id "")))
      (if-let ((fresh (cl-find id (funcall rows-fn)
                               :key (lambda (r) (alist-get 'id r)) :test #'equal)))
          (progn (table-view-upsert-row buf fresh) (message "Refreshed %s" id))
        (table-view-delete-row buf id)
        (message "%s is gone" id))
    (table-view-set-rows buf (funcall rows-fn))))

(defun table-view-ray--show (buf title columns sort rows-fn linkp)
  "Display table BUF titled TITLE with COLUMNS, SORT, filled by ROWS-FN.
Binds `g' to refresh the row at point and `G' to refresh all; with LINKP,
RET follows the Org link at point."
  (let* ((refresh (lambda (&optional id) (table-view-ray--refresh buf rows-fn id)))
         (actions (append
                   '(((key . "g") (label . "Refresh row") (command . "ray-refresh-one"))
                     ((key . "G") (label . "Refresh all") (command . "ray-refresh-all")))
                   (and linkp '(((key . "RET") (label . "Open") (command . "open-link"))))))
         (handlers (append
                    `(("ray-refresh-one" . ,(lambda (id _row) (funcall refresh id)))
                      ("ray-refresh-all" . ,(lambda (_id _row) (funcall refresh))))
                    (and linkp `(("open-link" . ,(lambda (_id _row) (table-view-open-link)))))))
         (spec `((title . ,title) (columns . ,columns) (sort . ,sort) (actions . ,actions))))
    (table-view-display buf spec handlers (lambda (_b) (funcall refresh)))
    (get-buffer buf)))

;;; Actors

(defconst table-view-ray--actor-columns
  '(((key . "actor")    (header . "Actor"))
    ((key . "class")    (header . "Class"))
    ((key . "name")     (header . "Name"))
    ((key . "state")    (header . "State") (type . "badge")
     (badges . (((value . "ALIVE")                (color . "#50fa7b"))
                ((value . "DEAD")                 (color . "#ff5555"))
                ((value . "RESTARTING")           (color . "#f1fa8c"))
                ((value . "PENDING_CREATION")     (color . "#8be9fd"))
                ((value . "DEPENDENCIES_UNREADY") (color . "#8be9fd")))))
    ((key . "restarts") (header . "Restarts") (type . "number") (align . "right"))
    ((key . "pid")      (header . "PID")      (type . "number") (align . "right"))
    ((key . "job")      (header . "Job"))))

(defun table-view-ray--rows-actors (cluster)
  "The actor rows of CLUSTER (server already sorts by state, then name)."
  (let* ((data (table-view-ray--get (format "/api/clusters/%s/actors" cluster)))
         (head (alist-get 'head_ip data)))
    (mapcar
     (lambda (a)
       (let* ((id (or (alist-get 'actor_id a) ""))
              (short (if (> (length id) 8) (substring id 0 8) id)))
         (list (cons 'id (if (string-empty-p id) short id))
               (cons 'cells
                     (list (cons 'actor    (table-view-ray--link head (format "actors/%s" id) short))
                           (cons 'class    (or (alist-get 'class_name a) ""))
                           (cons 'name     (or (alist-get 'name a) ""))
                           (cons 'state    (or (alist-get 'state a) ""))
                           (cons 'restarts (or (alist-get 'num_restarts a) 0))
                           (cons 'pid      (or (alist-get 'pid a) 0))
                           (cons 'job      (or (alist-get 'job_id a) "")))))))
     (alist-get 'actors data))))

;;;###autoload
(defun table-view-ray-actors (cluster)
  "Show the actors of a Ray CLUSTER in a table-view."
  (interactive (list (table-view-ray--read-cluster)))
  (table-view-ray--show (format "*ray-actors: %s*" cluster)
                        (format "Ray actors — %s" cluster)
                        table-view-ray--actor-columns
                        '((column . "state") (ascending . t))
                        (lambda () (table-view-ray--rows-actors cluster)) t))

;;; Jobs

(defconst table-view-ray--job-columns
  '(((key . "job")        (header . "Job"))
    ((key . "status")     (header . "Status") (type . "badge")
     (badges . (((value . "SUCCEEDED") (color . "#50fa7b"))
                ((value . "RUNNING")   (color . "#8be9fd"))
                ((value . "FAILED")    (color . "#ff5555"))
                ((value . "PENDING")   (color . "#f1fa8c"))
                ((value . "STOPPED")   (color . "#6272a4")))))
    ((key . "entrypoint") (header . "Entrypoint"))
    ((key . "submission") (header . "Submission"))
    ((key . "started")    (header . "Started"))
    ((key . "ended")      (header . "Ended"))))

(defun table-view-ray--rows-jobs (cluster)
  "The job rows of CLUSTER, newest first."
  (let ((head (table-view-ray--head cluster))
        (jobs (sort (table-view-ray--get (format "/api/clusters/%s/jobs" cluster))
                    (lambda (a b) (> (or (alist-get 'start_time a) 0)
                                     (or (alist-get 'start_time b) 0))))))
    (mapcar
     (lambda (j)
       (let ((jid (or (alist-get 'job_id j) "")))
         (list (cons 'id (if (string-empty-p jid) (or (alist-get 'submission_id j) "?") jid))
               (cons 'cells
                     (list (cons 'job        (table-view-ray--link head (format "jobs/%s" jid) jid))
                           (cons 'status     (or (alist-get 'status j) ""))
                           (cons 'entrypoint (table-view-ray--trunc (alist-get 'entrypoint j) 70))
                           (cons 'submission (or (alist-get 'submission_id j) ""))
                           (cons 'started    (table-view-ray--ts (alist-get 'start_time j)))
                           (cons 'ended      (table-view-ray--ts (alist-get 'end_time j))))))))
     jobs)))

;;;###autoload
(defun table-view-ray-jobs (cluster)
  "Show the jobs of a Ray CLUSTER in a table-view."
  (interactive (list (table-view-ray--read-cluster)))
  (table-view-ray--show (format "*ray-jobs: %s*" cluster)
                        (format "Ray jobs — %s" cluster)
                        table-view-ray--job-columns
                        '((column . "started") (ascending . nil))
                        (lambda () (table-view-ray--rows-jobs cluster)) t))

;;; Nodes

(defconst table-view-ray--node-columns
  '(((key . "node")    (header . "Node"))
    ((key . "role")    (header . "Role") (type . "badge")
     (badges . (((value . "head")   (color . "#8be9fd"))
                ((value . "worker") (color . "#6272a4")))))
    ((key . "cpu")     (header . "CPU%")  (type . "number") (align . "right"))
    ((key . "mem")     (header . "Mem%")  (type . "number") (align . "right"))
    ((key . "disk")    (header . "Disk%") (type . "number") (align . "right"))
    ((key . "running") (header . "Run")   (type . "number") (align . "right"))
    ((key . "pending") (header . "Pend")  (type . "number") (align . "right"))
    ((key . "updated") (header . "Updated"))))

(defun table-view-ray--rows-nodes (cluster)
  "The node rows of CLUSTER, head first then by name."
  (let* ((data (table-view-ray--get (format "/api/clusters/%s/nodes" cluster)))
         (head (alist-get 'head_ip data))
         (nodes (sort (alist-get 'nodes data)
                      (lambda (a b)
                        (let ((ha (eq (alist-get 'is_head a) t))
                              (hb (eq (alist-get 'is_head b) t)))
                          (if (eq ha hb)
                              (string< (or (alist-get 'node a) "") (or (alist-get 'node b) ""))
                            ha))))))
    (mapcar
     (lambda (n)
       (let ((node (or (alist-get 'node n) "")))
         (list (cons 'id node)
               (cons 'cells
                     (list (cons 'node    (table-view-ray--link head (format "cluster/nodes/%s" node) node))
                           (cons 'role    (if (eq (alist-get 'is_head n) t) "head" "worker"))
                           (cons 'cpu     (or (alist-get 'cpu_pct n) 0))
                           (cons 'mem     (or (alist-get 'mem_pct n) 0))
                           (cons 'disk    (or (alist-get 'disk_pct n) 0))
                           (cons 'running (or (alist-get 'running n) 0))
                           (cons 'pending (or (alist-get 'pending n) 0))
                           (cons 'updated (table-view-ray--ts (alist-get 'last_ts n))))))))
     nodes)))

;;;###autoload
(defun table-view-ray-nodes (cluster)
  "Show the nodes of a Ray CLUSTER in a table-view."
  (interactive (list (table-view-ray--read-cluster)))
  (table-view-ray--show (format "*ray-nodes: %s*" cluster)
                        (format "Ray nodes — %s" cluster)
                        table-view-ray--node-columns
                        '((column . "role") (ascending . t))
                        (lambda () (table-view-ray--rows-nodes cluster)) t))

;;; Operations (cluster management tasks)

(defconst table-view-ray--op-columns
  '(((key . "operation") (header . "Operation"))
    ((key . "status")    (header . "Status") (type . "badge")
     (badges . (((value . "completed") (color . "#50fa7b"))
                ((value . "running")   (color . "#8be9fd"))
                ((value . "failed")    (color . "#ff5555"))
                ((value . "pending")   (color . "#f1fa8c")))))
    ((key . "created")   (header . "Created"))
    ((key . "finished")  (header . "Finished"))
    ((key . "task")      (header . "Task Id"))))

(defun table-view-ray--rows-tasks (cluster)
  "The cluster-operation rows of CLUSTER, newest first."
  (let ((ops (sort (alist-get 'operations
                              (table-view-ray--get (format "/api/clusters/%s/operations" cluster)))
                   (lambda (a b) (> (or (alist-get 'created_at a) 0)
                                    (or (alist-get 'created_at b) 0))))))
    (mapcar
     (lambda (o)
       (let ((id (or (alist-get 'id o) "")))
         (list (cons 'id id)
               (cons 'cells
                     (list (cons 'operation (or (alist-get 'operation o) ""))
                           (cons 'status    (or (alist-get 'status o) ""))
                           (cons 'created   (table-view-ray--ts (alist-get 'created_at o)))
                           (cons 'finished  (table-view-ray--ts (alist-get 'finished_at o)))
                           (cons 'task      id))))))
     ops)))

;;;###autoload
(defun table-view-ray-tasks (cluster)
  "Show the cluster-management operations of a Ray CLUSTER in a table-view."
  (interactive (list (table-view-ray--read-cluster)))
  (table-view-ray--show (format "*ray-tasks: %s*" cluster)
                        (format "Ray operations — %s" cluster)
                        table-view-ray--op-columns
                        '((column . "created") (ascending . nil))
                        (lambda () (table-view-ray--rows-tasks cluster)) nil))

(provide 'ray-cluster)
;;; ray-cluster.el ends here
