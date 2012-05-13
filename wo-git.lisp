;;;; wo-git.lisp

(in-package #:wo-git)

;;; "wo-git" goes here. Hacks and glory await!


(defparameter *git-command*
  #+linux "/usr/bin/git"
  #-linux "/usr/local/bin/git"
  "The git executeable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to run git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-git (git-dir &rest args)
  "Runs the git executeable from *git-command*
with --git-dir=`git-dir' and the additional arguments speficied as a list of arguments
in `args'.

It returns a list of strings corresponding to the stdout of the git command.

It is not specified what happens if th git command throws an error.

An example of use would be:

  \(run-git git-dir \"log\" \"--pretty=format:%H %P\" \"--all\"\)
"

  (let ((proc (run-program *git-command*
			  (cons (format nil "--git-dir=~A" git-dir) args)
			  :output :stream
			  :wait nil)))
    (prog1
	(loop :for line = (read-line (process-output proc) nil)
	   :while line :collect line)
      (process-close proc))))


(defun oid-to-string (oid)
  (format nil "~40,'0X" oid))

(defun git-names ()
  "Returns a hash table which maps commit SHA strings to a list of names.
The precondition is that the git repository is already opened"
  (let ((result (make-hash-table :test #'equalp)))
    (loop :for reference-name :in (cl-git:git-reference-listall)
       :for reference = (cl-git:git-reference-lookup reference-name)
       :for resolved-reference = (cl-git:git-reference-resolve reference)
       :for oid = (cl-git:git-reference-oid resolved-reference)
       :do
       (push reference-name (gethash (oid-to-string oid) result (list)))
       (cl-git:git-object-free resolved-reference)
       (cl-git:git-object-free reference))
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
(defun get-git-graph (git-dir)
  (let ((graph (make-instance 'git-graph :test #'equalp)))
    (cl-git:with-git-repository (git-dir)
      (cl-git:with-git-revisions
	  (commit :head (cl-git:git-reference-listall))
	(loop :for parent :in (cl-git::git-commit-parent-oids commit)
	   :do
	   (add-edge (oid-to-string parent)
	    (oid-to-string (cl-git::git-object-id commit))
	    nil graph)))
      (setf (name-map graph) (git-names))
      (setf (reverse-name-map graph) (reverse-table (name-map graph))))
    ;; Add initial commits to the maps to:
    (loop
       :with name-map = (name-map graph)
       :with rn-map = (reverse-name-map graph)
       :with index = 0
       :for vertex :in (all-vertices graph)
       :unless (sources-of-vertex vertex graph) :do
       (let ((fake-name (format nil "Initial-~D" index)))
	 (incf index)
	 (setf (gethash fake-name rn-map) (list vertex))
	 (setf (gethash vertex name-map) (list fake-name))))
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
