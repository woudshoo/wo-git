;;;; wo-git.lisp

(in-package #:wo-git)

;;; "wo-git" goes here. Hacks and glory await!


(defparameter *git-command*
  #+linux "/usr/bin/git"
  #-linux "/usr/local/bin/git"
  "The git executeable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing git output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-parse-names (names)
  "Given the git output given by %d in the string `names',
which looks likea string like (name-1, name-2, ...)
return a list containing the name-1, name-2, ..."
  (loop :for name :in
     (mapcar #'trim-spaces
	     (split "\\(|,|\\)" names))
     :when (> (length name) 0) :collect name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to run git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-git (git-dir &rest args)
  "Runs the git executeable from *git-command*
with --git-dir=`git-dir' and the additional arguments speficied as a list of arguments
in `args'.

It returns a list of strings corresponding to the stdout of the git command.

It is not specified what happens if th git command trhows an error."

  (let ((proc (run-program *git-command*
			  (cons (format nil "--git-dir=~A" git-dir) args)
			  :output :stream
			  :wait nil)))
    (prog1
	(loop :for line = (read-line (process-output proc) nil)
	   :while line :collect line)
      (process-close proc))))

(defun git-commits (git-dir &rest selection)
  "Returns all commits as a list of
strings.  Each string looks like:

  SHA-commit SHA-commit-parent-1 ... SHA-commit-parent-n

The `git-dir' argument is the path to the repository for which we want
to know the commits and the selection is a list of command line arguments
which can be used to select a subset of commits.  For valid
values of `selection' read up on the git man page.
"
  (apply #'run-git git-dir  "log" "--pretty=format:%H %P" selection))

(defun git-names (git-dir &rest selection)
  "Returns a hash table which maps commit SHA strings to a list of names.
A name is either a tag or a branch.
The argument `git-dir' is the path to the repository and the `selection' is used to specify which commits to include."
  (let ((result (make-hash-table :test #'equalp)))
    (loop :for line :in (apply #'run-git git-dir "log" "--pretty=format:%H %d" selection)
       :do
       (setf (gethash (subseq line 0 40) result)
	     (git-parse-names (subseq line 40))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass git-graph (simple-graph)
  ((name-map :accessor name-map)
   (reverse-name-map :accessor reverse-name-map)))


(defmethod copy ((graph git-graph))
  (let ((result (call-next-method graph)))
     (wo-graph::copy-hash-table-slot result graph 'name-map)
     (wo-graph::copy-hash-table-slot result graph 'reverse-name-map)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creation of the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+nil (defun get-git-graph (git-dir)
  (let ((graph (make-instance 'git-graph :test #'equalp)))
    (loop :for line :in (git-commits git-dir "--all")
       :for split-line = (cl-ppcre:split " " line)
       :do
       (loop :with commit = (car split-line)
	  :for parent :in (cdr split-line)
	  :do
	  (add-edge parent commit nil graph)))
    (setf (name-map graph) (git-names git-dir "--all"))
    (setf (reverse-name-map graph) (reverse-table (name-map graph)))
    graph))

(defun get-git-graph (git-dir)
  (let ((graph (make-instance 'git-graph :test #'equalp)))
    (cl-git:with-git-repository (git-dir)
      (cl-git:with-git-revisions
	  (commit :head (cl-git:git-reference-listall))
	(loop :for parent :in (cl-git::git-commit-parent-oids commit)
	   :do
	   (add-edge
	    (cl-git::git-oid-tostr parent)
	    (cl-git::git-oid-tostr (cl-git::git-object-id commit))
	    nil graph))))
    (setf (name-map graph) (git-names git-dir "--all"))
    (setf (reverse-name-map graph) (reverse-table (name-map graph)))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic inquiry git graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-names (git-graph)
  "Returns a list of all known names of `git-graph'."
  (hash-table-keys (reverse-name-map git-graph)))


(defun match-name (name git-graph)
  "Returns a vertex which has a name most closely matching `name'"
  (minimizing (all-names git-graph)
	      (lambda (candidate) (cdr (editing-distance name candidate :anchored nil)))))

(defun vertex-names (vertex git-graph)
  "Returns a list of all git names given to the `vertex'."
  (gethash vertex  (name-map git-graph)))


(defun name-to-vertex (name git-graph)
  "Returns the vertex which has git name `name'.  Or nil if it does not exist."
  (car (gethash name (reverse-name-map git-graph))))

(defun name-or-rev-to-vertex (name-or-rev git-graph)
  "Returns the vertex corresponding to `name-or-rev'.
The argument `name-or-rev' can either be a full SHA1 commit (as string)
or a name for the commit.  If there is a name with which is also
a SHA1 for a different commit, the named vertex will be returned."
  (or (name-to-vertex name-or-rev git-graph) name-or-rev))
