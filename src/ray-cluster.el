;;; ray-cluster.el --- Ray cluster actors in a table-view -*- lexical-binding: t; -*-

;; Author: Dmitry Akatov <akatovda@gmail.com>

;;; Commentary:
;; `M-x table-view-ray-actors' asks (with completion, match required) which Ray
;; cluster to inspect, then shows that cluster's actors -- id, class, name,
;; state, restarts, pid, job -- in a table-view.
;;
;; Data comes from the ray-cluster-manager service:
;;   GET /api/clusters/            -> the cluster list (pick target)
;;   GET /api/clusters/NAME/actors -> {head_ip, actors:[...]}
;; Point `table-view-ray-base-url' at the manager.  The actor id cell is an Org
;; link to that actor's page in the Ray dashboard (http://HEAD_IP:8265/#/actors/…);
;; press RET or C-c C-o on it, or mouse-1.  `g' refetches the current cluster.

;;; Code:

(require 'table-view)
(require 'url)
(require 'json)

(defgroup table-view-ray nil
  "Ray cluster actors in a table-view." :group 'table-view :prefix "table-view-ray-")

(defcustom table-view-ray-base-url "http://10.17.5.9:8000"
  "Base URL of the ray-cluster-manager service." :type 'string)

(defcustom table-view-ray-dashboard-port 8265
  "Ray dashboard port, used to build actor-view links." :type 'integer)

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

(defun table-view-ray--actor-row (actor head-ip)
  "Build a table-view row for ACTOR.
Its id cell links to the Ray dashboard at HEAD-IP."
  (let* ((id (or (alist-get 'actor_id actor) ""))
         (short (if (> (length id) 8) (substring id 0 8) id))
         (id-cell (if (and (not (string-empty-p id)) head-ip (not (string-empty-p head-ip)))
                      (format "[[http://%s:%d/#/actors/%s][%s]]"
                              head-ip table-view-ray-dashboard-port id short)
                    short)))
    (list (cons 'id (if (string-empty-p id) short id))
          (cons 'cells
                (list (cons 'actor    id-cell)
                      (cons 'class    (or (alist-get 'class_name actor) ""))
                      (cons 'name     (or (alist-get 'name actor) ""))
                      (cons 'state    (or (alist-get 'state actor) ""))
                      (cons 'restarts (or (alist-get 'num_restarts actor) 0))
                      (cons 'pid      (or (alist-get 'pid actor) 0))
                      (cons 'job      (or (alist-get 'job_id actor) "")))))))

(defconst table-view-ray--columns
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

(defun table-view-ray--fill (buffer cluster)
  "Fetch CLUSTER's actors and load them into table-view BUFFER."
  (let* ((data (table-view-ray--get (format "/api/clusters/%s/actors" cluster)))
         (head-ip (alist-get 'head_ip data)))
    (table-view-set-rows buffer
                         (mapcar (lambda (a) (table-view-ray--actor-row a head-ip))
                                 (alist-get 'actors data)))))

;;;###autoload
(defun table-view-ray-actors (cluster)
  "Show the actors of a Ray CLUSTER in a table-view.
Reads the cluster list from `table-view-ray-base-url' and prompts for one
(completion, match required)."
  (interactive
   (list (completing-read
          "Ray cluster: "
          (mapcar (lambda (c) (alist-get 'name c)) (table-view-ray--get "/api/clusters/"))
          nil t)))
  (let ((buf (format "*ray-actors: %s*" cluster))
        (spec `((title . ,(format "Ray actors — %s" cluster))
                (columns . ,table-view-ray--columns)
                (sort . ((column . "state") (ascending . t)))))
        (handlers `(("open-link" . ,(lambda (_id _row) (table-view-open-link))))))
    ;; A fill-fn (re-run by `g') keeps the actor list refreshable.
    (table-view-display buf (append spec '((actions . (((key . "RET") (label . "Open actor")
                                                        (command . "open-link"))))))
                        handlers
                        (lambda (b) (table-view-ray--fill b cluster)))
    (get-buffer buf)))

(provide 'ray-cluster)
;;; ray-cluster.el ends here
