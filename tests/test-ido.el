;;; -*- lexical-binding: t -*-

(require 'f)
(require 'ido)
(require 'cl-lib)
(require 'buttercup)
(require 'with-simulated-input)

(message "Ido location: %S" (locate-library "ido"))

;; Note: Currently unused, but potentially useful in the future
(defun ido-cr+-maybe-chop (items elem)
  "Like `ido-chop', but a no-op if ELEM is not in ITEMS.

Normal `ido-chop' hangs infinitely in this case."
  (cl-loop
   with new-tail = ()
   for remaining on items
   for next = (car remaining)
   if (equal next elem)
   return (nconc remaining new-tail)
   else collect next into new-tail
   finally return items))

(defun collection-as-function (collection)
  "Return a function equivalent to COLLECTION.

The returned function will work equivalently to COLLECTION when
passed to `all-completions' and `try-completion'."
  (completion-table-dynamic (lambda (string) (all-completions string collection))))

(defun shadow-var (var &optional temp-value)
  "Shadow the value of VAR.

This will push the current value of VAR to VAR's
`shadowed-values' property, and then set it to TEMP-VALUE. To
reverse this process, call `unshadow-var' on VAR. Vars can
be shadowed recursively, and must be unshadowed once for each
shadowing in order to restore the original value. You can think
of shadowing as dynamic binding with `let', but with manual
control over when bindings start and end.

If VAR is a Custom variable (see `custom-variable-p'), it will be
set using `customize-set-variable', and if TEMP-VALUE is nil it
will be replaces with VAR's standard value.

 Other variables will be set with `set-default', and a TEMP-VALUE
 of nil will not be treated specially.

`shadow-var' only works on variables declared as special (i.e.
using `defvar' or similar). It will not work on lexically bound
variables."
  (unless (special-variable-p var)
    (error "Cannot shadow lexical var `%s'" var))
  (let* ((use-custom (custom-variable-p var))
         (setter (if use-custom 'customize-set-variable 'set-default))
         (temp-value (or temp-value
                         (and use-custom
                              (eval (car (get var 'standard-value)))))))
    ;; Push the current value on the stack
    (push (symbol-value var) (get var 'shadowed-values))
    (funcall setter var temp-value)))

(defun var-shadowed-p (var)
  "Return non-nil if VAR is shadowed by `shadow-var'."
  ;; We don't actually want to return that list if it's non-nil.
  (and (get var 'shadowed-values) t))

(defun unshadow-var (var)
  "Reverse the last call to `shadow-var' on VAR."
  (if (var-shadowed-p var)
      (let* ((use-custom (custom-variable-p var))
             (setter (if use-custom 'customize-set-variable 'set-default))
             (value (pop (get var 'shadowed-values))))
        (funcall setter var value))
    (error "Var is not shadowed: %s" var)))

(defun fully-unshadow-var (var)
  "Reverse *all* calls to `shadow-var' on VAR."
  (when (var-shadowed-p var)
    (let* ((use-custom (custom-variable-p var))
           (setter (if use-custom 'customize-set-variable 'set-default))
           (value (car (last (get var 'shadowed-values)))))
      (put var 'shadowed-values nil)
      (funcall setter var value))))

(defun fully-unshadow-all-vars (&optional vars)
  "Reverse *all* calls to `shadow-var' on VARS.

If VARS is nil, unshadow *all* variables."
  (if vars
      (mapc #'fully-unshadow-var vars)
    (mapatoms #'fully-unshadow-var))
  nil)

(defmacro shadow-vars (varlist)
  "Shadow a list of vars with new values.

VARLIST describes the variables to be shadowed with the same
syntax as `let'.

See `shadow-var'."
  (declare (indent 0))
  (cl-loop
   with var = nil
   with value = nil
   for binding in varlist
   if (symbolp binding)
   do (setq var binding
            value nil)
   else
   do (setq var (car binding)
            value (cadr binding))
   collect `(shadow-var ',var ,value) into exprs
   finally return `(progn ,@exprs)))

(defmacro unshadow-vars (vars)
  "Un-shadow a list of VARS.

This is a macro for consistency with `shadow-vars', but it will
also accept a quoted list for the sake of convenience."
  (declare (indent 0))
  (when (eq (car vars) 'quote)
    (setq vars (eval vars)))
  `(mapc #'unshadow-var ',vars))

(defmacro with-temp-info-buffer (&rest body)
  "Create a temporary info buffer and exeluate BODY forms there."
  (declare (indent 0))
  `(let ((temp-bufname (generate-new-buffer-name " *temp-info*")))
     (unwind-protect
         (save-excursion
           (info nil (generate-new-buffer-name " *temp-info*"))
           ,@body)
       (when (get-buffer temp-bufname)
         (kill-buffer temp-bufname)))))

(defmacro with-temp-dir (&rest body)
  "Evaluate BODY in a temporary directory.

While BODY is evaluating, `default-directory' will be set to the
temporary directory, and then the directory will be deleted when
BODY is done executing."
  (declare (indent 0))
  `(let* ((tmpdir (make-temp-file nil t))
          (default-directory tmpdir))
     (unwind-protect
         (progn ,@body)
       (when (f-exists? tmpdir)
         (f-delete tmpdir t)))))

(defun f-same-path? (path-a path-b)
  "Like `f-same?' but works on nonexistent paths.

This will return non-nil for any paths that `f-same?' returns
non-nil on, but in addition, it will return non-nil if PATH-A and
PATH-B can be positively identified as equivalent paths to the
same nonexistent file."
  (equal
   (f-canonical (directory-file-name (f-expand path-a)))
   (f-canonical (directory-file-name (f-expand path-b)))))

(defmacro with-named-temp-buffers (buffer-names &rest body)
  "Create several named buffers, then eval BODY, then kill them.

If any of BUFFER-NAMES is the same as an existing buffer, this
function avoids killing that buffer at the end of BODY. "
  (declare (indent 1))
  `(let ((created-buffers nil))
     (unwind-protect
         (progn
           (cl-loop
            for bufname in ',buffer-names
            if (get-buffer bufname) do (ignore)
            else do (push (get-buffer-create bufname) created-buffers))
           ,@body)
       (cl-loop for buf in created-buffers
                do (kill-buffer buf)))))

(describe "ido"

  ;; Reset all of these variables to their standard values before each
  ;; test, saving the previous values for later restoration.
  (before-each
    (shadow-vars
     ((ido-mode t)
      ido-confirm-unique-completion
      ido-enable-flex-matching
      ido-enable-dot-prefix))

    ;; Suppress all messages during tests
    (spy-on 'message))

  ;; Restore the saved values after each test
  (after-each
    (fully-unshadow-all-vars))

  (describe "ido-completing-read"

    (it "should accept a matching completion"
      (expect
       (with-simulated-input "green RET"
         (ido-completing-read "Prompt: " '("blue" "yellow" "green")))
       :to-equal "green"))

    (it "should complete with a matching item on RET"
      (expect
       (with-simulated-input "g RET"
         (ido-completing-read "Prompt: " '("blue" "yellow" "green")))
       :to-equal "green"))

    (it "should complete with the first match when multiple matches are available"
      (expect
       (with-simulated-input "b RET"
         (ido-completing-read "Prompt: " '("yellow" "brown" "blue" "green")))
       :to-equal "brown"))

    (it "should allow <left> and <right> to cycle completions, with wrap-around"
      (expect
       (with-simulated-input "b <right> <right> <right> <right> <left> RET"
         (ido-completing-read "Prompt: " '("brown" "blue" "yellow" "green")))
       :to-equal
       "blue"))

    (it "should allow C-j to exit with a partial completion"
      (expect
       (with-simulated-input "b C-j"
         (ido-completing-read "Prompt: " '("yellow" "brown" "blue" "green")))
       :to-equal "b")))

  (describe "ido file operations"

    (describe "ido-read-file-name"
      (it "Should allow selecting an existing file with RET"
        (with-temp-dir
          ;; Let's create some files
          (write-region "" nil "a.txt")
          (write-region "" nil "b.txt")
          (write-region "" nil "c.txt")

          (expect
           (f-same-path?
            (with-simulated-input "RET"
              (ido-read-file-name "Pick a file: "))
            "a.txt"))
          (expect
           (f-same-path?
            (with-simulated-input "b.txt RET"
              (ido-read-file-name "Pick a file: "))
            "b.txt"))
          (expect
           (f-same-path?
            (with-simulated-input "b RET"
              (ido-read-file-name "Pick a file: "))
            "b.txt"))
          (expect
           (f-same-path?
            (with-simulated-input "<right> RET"
              (ido-read-file-name "Pick a file: "))
            "b.txt"))))

      (it "Should allow falling back to default completion with C-f"
        (spy-on 'read-file-name :and-call-through)
        (with-temp-dir
          ;; Let's create some files
          (write-region "" nil "a.txt")
          (write-region "" nil "b.txt")
          (write-region "" nil "c.txt")

          (expect
           (f-same-path?
            (with-simulated-input "b C-f RET"
              (ido-read-file-name "Pick a file: "))
            "b"))
          (expect
           (f-same-path?
            (with-simulated-input "C-f b RET"
              (ido-read-file-name "Pick a file: "))
            "b"))
          (expect
           (f-same-path?
            (with-simulated-input "b C-f TAB RET"
              (ido-read-file-name "Pick a file: "))
            "b.txt"))
          (expect
           (f-same-path?
            (with-simulated-input "C-f b.txt RET"
              (ido-read-file-name "Pick a file: "))
            "b.txt"))
          (expect 'read-file-name :to-have-been-called))))

    (describe "ido-read-directory-name"
      (it "Should allow selecting an existing directory with RET"
        (with-temp-dir
          ;; Let's create some files
          (write-region "" nil "a.txt")
          (write-region "" nil "b.txt")
          (write-region "" nil "c.txt")
          ;; Let's create some dirs
          (make-directory "d.dir")
          (make-directory "e.dir")
          (make-directory "f.dir")

          (expect
           (f-same-path?
            (with-simulated-input "RET"
              (ido-read-directory-name "pick a dir: "))
            default-directory))
          (expect
           (f-same-path?
            (with-simulated-input "e.dir RET RET"
              (ido-read-directory-name "pick a dir: "))
            "e.dir"))
          (expect
           (f-same-path?
            (with-simulated-input "e RET RET"
              (ido-read-directory-name "pick a dir: "))
            "e.dir"))
          (expect
           (f-same-path?
            (with-simulated-input "<right> RET RET"
              (ido-read-directory-name "pick a dir: "))
            "d.dir"))))

      (it "Should not allow selecting an existing non-directory file with RET"
        (with-temp-dir
          ;; Let's create some files
          (write-region "" nil "a.txt")
          (write-region "" nil "b.txt")
          (write-region "" nil "c.txt")
          ;; Let's create some dirs
          (make-directory "d.dir")
          (make-directory "e.dir")
          (make-directory "f.dir")

          (expect
           (with-simulated-input "a RET"
             (ido-read-directory-name "pick a dir: " nil nil t))
           :to-throw 'error))))

    (describe "ido-find-file"
      (it "should allow finding an existing file with RET"
        (with-temp-dir
          ;; Let's create some files
          (write-region "" nil "a.txt")
          (write-region "" nil "b.txt")
          (write-region "" nil "c.txt")
          ;; Let's create some dirs
          (make-directory "d.dir")
          (make-directory "e.dir")
          (make-directory "f.dir")

          (expect
           (f-same-path?
            (save-window-excursion
              (with-simulated-input "RET"
                (call-interactively 'ido-find-file))
              buffer-file-name)
            "a.txt"))

          (expect
           (f-same-path?
            (save-window-excursion
              (with-simulated-input "b RET"
                (call-interactively 'ido-find-file))
              buffer-file-name)
            "b.txt"))))

      (it "should allow finding new files"
        (with-temp-dir
          ;; Let's create some files
          (write-region "" nil "a.txt")
          (write-region "" nil "b.txt")
          (write-region "" nil "c.txt")
          ;; Let's create some dirs
          (make-directory "d.dir")
          (make-directory "e.dir")
          (make-directory "f.dir")

          (expect
           (f-same-path?
            (save-window-excursion
              (with-simulated-input "b C-j"
                (call-interactively 'ido-find-file))
              buffer-file-name)
            "b"))
          (expect
           (f-same-path?
            (save-window-excursion
              (with-simulated-input "g RET RET"
                (call-interactively 'ido-find-file))
              buffer-file-name)
            "g"))
          (expect
           (f-same-path?
            (save-window-excursion
              (with-simulated-input "e / hello C-j"
                (call-interactively 'ido-find-file))
              buffer-file-name)
            "e.dir/hello")))))
    (describe "ido-write-file"
      (it "should allow writing new files with RET RET"
        (with-temp-dir
          (with-temp-buffer
            (insert "Contents of x.txt")
            (with-simulated-input "x.txt RET RET"
              (call-interactively 'ido-write-file)))
          (expect (f-exists? "x.txt"))))

      ;; Can't get this one to work for some reason
      (xit "should allow overwriting existing files with RET y"
        (with-temp-dir
          (write-region "Old contents of b.txt" nil "b.txt")
          (expect (f-read-text "b.txt")
                  :to-match "Old contents")
          (with-temp-buffer
            (insert "New contents of b.txt")
            (setq buffer-file-name (f-canonical "b.txt"))
            (with-simulated-input "b.txt RET y"
              (call-interactively 'ido-write-file)))
          (expect (f-read-text "b.txt")
                  :to-match "New contents"))))
    (describe "ido-dired"))
  (describe "ido buffer operations"
    (describe "ido-read-buffer"

      (it "should allow selecting an existing buffer with RET"
        (expect
         (with-named-temp-buffers ("ido-test-buf-A" "ido-test-buf-B" "ido-test-buf-C")
           (with-simulated-input "ido-test-buf- RET"
             (ido-read-buffer "Pick a buffer: " nil t
                              ;; TODO: ido-read-buffer ignores predicate???
                              (lambda (item) (string-match-p "\\`ido-test-" (car-safe item))))))
         :to-match "\\`ido-test-buf-.\\'"))
      (it "should allow selecting a new buffer name with RET RET"
        (expect
         (with-named-temp-buffers ("ido-test-buf-A" "ido-test-buf-B" "ido-test-buf-C")
           (with-simulated-input "ido-test-buf-D RET RET"
             (ido-read-buffer "Pick a buffer: ")))
         :to-equal "ido-test-buf-D"))
      (it "should allow falling back to default completion with C-f"
        (spy-on 'read-buffer :and-call-through)
        (expect
         (with-named-temp-buffers ("ido-test-buf-A" "ido-test-buf-B" "ido-test-buf-C")
           (with-simulated-input "ido-test-buf- C-x C-b RET"
             (ido-read-buffer "Pick a buffer: ")))
         :to-equal "ido-test-buf-")
        (expect
         (with-named-temp-buffers ("ido-test-buf-A" "ido-test-buf-B" "ido-test-buf-C")
           (with-simulated-input "ido-test-buf-D C-x C-b RET"
             (ido-read-buffer "Pick a buffer: ")))
         :to-equal "ido-test-buf-D")
        (expect 'read-buffer :to-have-been-called)))
    ;; Use `save-window-excursion' liberally
    (describe "ido-switch-buffer"
      (it "should allow switching to an existing buffer with RET"
        (save-window-excursion
          (expect
           (with-named-temp-buffers ("ido-test-buf-A" "ido-test-buf-B" "ido-test-buf-C")
             (with-simulated-input "ido-test-buf- RET"
               (call-interactively 'ido-switch-buffer))
             (buffer-name))
           :to-match "\\`ido-test-buf-.\\'")))
      (it "should allow creating a new buffer with RET RET"
        (spy-on 'read-buffer :and-call-through)
        (expect
         (with-named-temp-buffers ("ido-test-buf-A" "ido-test-buf-B" "ido-test-buf-C")
           (with-simulated-input "ido-test-buf-D RET RET"
             (call-interactively 'ido-switch-buffer))
           (prog1 (buffer-name)
             (kill-buffer)))
         :to-equal "ido-test-buf-D"))))

  (describe "regression tests"

    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19412
    (xit "should not exhibit bug #19412"

      (with-temp-dir
        ;; Let's create some dirs
        (make-directory "d.dir")
        (make-directory "e.dir")
        (make-directory "f.dir")
        ;; Let's create some files in those dirs
        (write-region "" nil (f-join "d.dir" "a.txt"))
        (write-region "" nil (f-join "e.dir" "b.txt"))
        (write-region "" nil (f-join "f.dir" "c.txt"))

        ;; This works
        (expect
         (f-same?
          (with-simulated-input "e.dir/ C-j"
            (ido-read-file-name
             "Pick a file: "
             default-directory
             (f-join "d.dir" "a.txt")))
          (f-join default-directory "e.dir")))

        ;; This doesn't work
        (expect
         (f-same?
          (with-simulated-input "e.dir/ C-f RET"
            (ido-read-file-name
             "Pick a file: "
             default-directory
             (f-join "d.dir" "a.txt")))
          (f-join default-directory "e.dir")))

        ;; This also doesn't work
        (with-temp-buffer
          (let ((buffer-file-name "d.dir/a.txt"))
            (expect
             (with-simulated-input "e.dir/ C-f RET"
               (call-interactively 'ido-write-file))
             :not :to-throw)
            (expect (f-exists? "e.dir/a.txt"))))))

    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=11861
    (xit "should not exhibit bug #11861")

    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=1516
    (xit "should not exhibit bug #1516")))

;;; test-ido.el ends here
