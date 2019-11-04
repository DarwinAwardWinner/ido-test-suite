;;; -*- lexical-binding: t -*-

(require 'ido)
(require 'cl-lib)
(require 'buttercup)
(require 'with-simulated-input)

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

  (describe "basic functionality"

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

  (describe "regression tests"

    (it "should not exhibit bug #19412"
      (expect
       (with-simulated-input "C-f RET"
         (ido-read-file-name
          "Pick a file (default ~/temp/test.R): "
          "~/" "~/temp/test.R"))
       :to-equal "~/temp/test.R")

      (expect
       (with-simulated-input "/ t m p / C-f RET"
         (ido-read-file-name
          "Pick a file (default ~/temp/test.R): "
          "~/" "~/temp/test.R"))
       :to-equal "/tmp/")

      (expect
       (cl-letf*
           ;; Redefine `write-file' as a no-op stub with the same
           ;; interactive form that just returns its argument
           ((write-file-intform (interactive-form 'write-file))
            ((symbol-function 'write-file)
             `(lambda (filename &optional confirm) ,write-file-intform
                (message "Would have written file: %S" filename)
                filename)))
         (with-temp-buffer
           (setq default-directory "~/temp/"
                 buffer-file-name "~/temp/test.R")
           (with-simulated-input "/ / t m p / C-f RET"
             (call-interactively 'ido-write-file))))
       :to-equal "/tmp/"))))

;;; test-ido.el ends here
