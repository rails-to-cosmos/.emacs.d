import importlib.machinery
from pathlib import Path
import tarfile
import tempfile
import unittest
from unittest.mock import patch

pipeline = importlib.machinery.SourceFileLoader('package_update', str(Path(__file__).resolve().parents[1] / 'scripts/package-update')).load_module()


class SecurityTests(unittest.TestCase):
    def test_lock_detects_version_and_content_changes(self):
        with tempfile.TemporaryDirectory() as temp:
            tree = Path(temp)
            package = tree / 'demo-1.0'
            package.mkdir()
            descriptor = package / 'demo-pkg.el'
            descriptor.write_text('(define-package "demo" "1.0" "test")')
            baseline = pipeline.lock_text(tree)
            descriptor.write_text('(define-package "demo" "2.0" "test")')
            self.assertNotEqual(baseline, pipeline.lock_text(tree))
            descriptor.write_text('(define-package "demo" "1.0" "test")')
            (package / 'payload.el').write_text('(shell-command "bad")')
            self.assertNotEqual(baseline, pipeline.lock_text(tree))

    def test_tar_rejects_traversal_and_links(self):
        for name, kind in [('demo-1/../../escape', tarfile.REGTYPE), ('demo-1/link', tarfile.SYMTYPE)]:
            with self.subTest(name=name), tempfile.TemporaryDirectory() as temp:
                archive = Path(temp) / 'demo.tar'
                with tarfile.open(archive, 'w') as out:
                    member = tarfile.TarInfo(name)
                    member.type = kind
                    member.linkname = '/tmp/escape'
                    out.addfile(member)
                with self.assertRaises(ValueError):
                    pipeline.unpack(archive, Path(temp) / 'out', 'demo-1')

    def test_flags_only_added_lines(self):
        self.assertEqual([], pipeline.flags(' context (call-process "sh")\n-(shell-command "ls")'))
        self.assertGreater(len(pipeline.flags('+(call-process "sh")')), len(pipeline.flags('+(message "hello")')))

    def test_staged_tampering_rejected(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(pipeline, 'STAGE', Path(temp)):
            stage = Path(temp)
            (stage / 'downloads').mkdir()
            (stage / 'downloads/demo-1.el').write_text('changed')
            (stage / 'manifest.json').write_text('[{"name":"demo","version":"1","file":"demo-1.el","sha256":"wrong"}]')
            with self.assertRaises(ValueError):
                pipeline.manifest()

    def test_failed_apply_restores_snapshot(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(pipeline, 'ROOT', Path(temp)), patch.object(pipeline, 'STAGE', Path(temp) / 'stage'):
            root = Path(temp)
            (root / '.cache').mkdir()
            (root / 'elpa').mkdir()
            (root / 'stage').mkdir()
            (root / 'stage/baseline.el').write_text('baseline')
            from contextlib import contextmanager
            @contextmanager
            def fake_container(work, *args):
                (work / 'result').mkdir()
                yield 'fake'
            def fake_copy(destination):
                destination.mkdir()
            def fake_run(*args, **kwargs):
                if args[0] == 'make':
                    raise RuntimeError('broken package')
            with patch.object(pipeline, 'manifest', return_value=[]), patch.object(pipeline, 'lock_text', return_value='baseline'), patch.object(pipeline, 'diffs'), patch.object(pipeline, 'copy_packages', side_effect=fake_copy), patch.object(pipeline, 'container', fake_container), patch.object(pipeline, 'run', side_effect=fake_run), patch.object(pipeline, 'snapshot', return_value='snapshot'), patch.object(pipeline, 'restore') as restore:
                with self.assertRaisesRegex(RuntimeError, 'broken package'):
                    pipeline.apply()
                restore.assert_called_once_with('snapshot')

    def test_snapshot_restores_tree_and_lock(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / 'elpa/demo-1').mkdir(parents=True)
            (root / '.cache').mkdir()
            (root / 'lock').mkdir()
            (root / 'elpa/demo-1/demo.el').write_text('original')
            (root / 'lock/packages.el').write_text('original lock')
            with patch.object(pipeline, 'ROOT', root), patch.object(pipeline, 'SNAPSHOTS', root / 'snapshots'):
                archive = pipeline.snapshot()
                (root / 'elpa/demo-1/demo.el').write_text('broken update')
                (root / 'lock/packages.el').write_text('changed lock')
                pipeline.restore(archive)
                self.assertEqual('original', (root / 'elpa/demo-1/demo.el').read_text())
                self.assertEqual('original lock', (root / 'lock/packages.el').read_text())

    def test_rebuild_resolves_downgrades_and_new_dependencies(self):
        with tempfile.TemporaryDirectory() as temp:
            source = Path(temp) / 'fetch.el'
            source.write_text(pipeline.FETCH_LISP)
            expression = r'''
(progn
  (require 'package)
  (require 'cl-lib)
  (let* ((package--initialized t)
         (package-alist (list (list 'demo (package-desc-create :name 'demo :version '(20260101)))))
         (package-archive-contents
          (list (list 'demo (package-desc-create :name 'demo :version '(2) :reqs '((dep (1)))))
                (list 'dep (package-desc-create :name 'dep :version '(1)))))
         form)
    (with-temp-buffer
      (insert-file-contents SOURCE)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((candidate (read (current-buffer))))
              (when (eq (car-safe candidate) 'let*) (setq form candidate))))
        (end-of-file nil)))
    (cl-letf (((symbol-function 'getenv) (lambda (&rest _) "1")))
      (let ((names (eval (list 'let* (cadr form)
                              '(mapcar #'package-desc-name transaction)) t)))
        (unless (equal names '(dep demo))
          (error "Expected dependency then downgraded root; got %S" names))))))
'''.replace('SOURCE', pipeline.json.dumps(str(source)))
            pipeline.run('emacs', '--batch', '-Q', '--eval', expression, capture_output=True, text=True)


class UpgradeTests(unittest.TestCase):
    def setUp(self):
        from contextlib import ExitStack
        from io import StringIO
        from unittest.mock import Mock
        self.stack = ExitStack()
        self.addCleanup(self.stack.close)
        self.stack.enter_context(patch.object(pipeline.sys.stdin, 'isatty', return_value=True))
        self.stack.enter_context(patch.object(pipeline.sys, 'stdout', StringIO()))
        self.stack.enter_context(patch.object(pipeline.sys.stdout, 'isatty', return_value=True))
        self.calls = Mock()
        for name in ('verify_lock', 'fetch', 'manifest', 'report', 'apply'):
            mock = self.stack.enter_context(patch.object(pipeline, name))
            self.calls.attach_mock(mock, name)
        pipeline.manifest.return_value = [{'name': 'demo', 'sha256': 'reviewed'}]
        self.answer = self.stack.enter_context(patch('builtins.input', return_value='apply'))
        self.calls.attach_mock(self.answer, 'review')

    def test_approval_follows_verification_and_report(self):
        pipeline.upgrade()
        self.assertEqual(['verify_lock', 'fetch', 'manifest', 'report', 'review', 'manifest', 'verify_lock', 'apply'],
                         [call[0] for call in self.calls.mock_calls])

    def test_decline_or_eof_never_installs(self):
        for answer in ('', 'yes', EOFError()):
            with self.subTest(answer=answer):
                self.answer.side_effect = answer if isinstance(answer, Exception) else None
                self.answer.return_value = answer
                pipeline.upgrade()
                pipeline.apply.assert_not_called()

    def test_noninteractive_run_stops_before_fetch(self):
        pipeline.sys.stdin.isatty.return_value = False
        with self.assertRaisesRegex(SystemExit, 'interactive terminal'):
            pipeline.upgrade()
        pipeline.fetch.assert_not_called()
        pipeline.apply.assert_not_called()

    def test_no_updates_never_prompts_or_installs(self):
        pipeline.manifest.return_value = []
        pipeline.upgrade()
        self.answer.assert_not_called()
        pipeline.apply.assert_not_called()

    def test_verification_or_download_failure_never_installs(self):
        for name in ('verify_lock', 'fetch'):
            with self.subTest(name=name):
                operation = getattr(pipeline, name)
                operation.side_effect = RuntimeError('verification failed')
                with self.assertRaisesRegex(RuntimeError, 'verification failed'):
                    pipeline.upgrade()
                operation.side_effect = None
                self.answer.assert_not_called()
                pipeline.apply.assert_not_called()

    def test_changed_manifest_requires_new_review(self):
        pipeline.manifest.side_effect = [[{'sha256': 'reviewed'}], [{'sha256': 'changed'}]]
        with self.assertRaisesRegex(SystemExit, 'changed during review'):
            pipeline.upgrade()
        pipeline.apply.assert_not_called()
