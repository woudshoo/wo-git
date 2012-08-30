;;;; wo-git.lisp

(in-package #:wo-git)

;;; "wo-git" goes here. Hacks and glory await!


(defparameter *git-command*
  #+linux "/usr/bin/git"
  #-linux "/usr/bin/git"
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


(defun git-names ()
  "Returns a hash table which maps commit oid to a list of names.
The precondition is that the git repository is already opened"
  (let ((result (make-hash-table :test #'equalp)))
    (loop :for reference-name :in (cl-git:git-reference-listall :SYMBOLIC :OID :PACKED)
       :for reference = (cl-git:git-reference-lookup reference-name)
       :for resolved-reference = (cl-git:git-reference-resolve reference)
       :for oid = (cl-git:git-reference-oid resolved-reference)
       :for obj = (ignore-errors (cl-git:git-object-lookup oid :any))
       :do
       (when obj
	 (case (cl-git:git-object-type obj)
	   (:tag (setf obj (cl-git:tag-target obj))))
	 (push reference-name (gethash (cl-git:git-object-id obj) result (list))))
       (cl-git:git-object-free resolved-reference)
       (cl-git:git-object-free reference))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass git-graph (simple-graph)
  ((name-map :accessor name-map)
   (reverse-name-map :accessor reverse-name-map))
  (:documentation
   "Representing the git revision graphs.  Nodes are OIDs (which are integers)
and the vertices are the default vertices (that is #(OID1 OID2)).

It also has two maps, one from name to OID and one from OID to name.

A name can be any name refering to an OID, but the default is to have all tags and branches
as a name, in addition to the generated names of the form `Initial-0' etc which name
the initial commits in the repository."))


(defmethod copy ((graph git-graph))
  (let ((result (call-next-method graph)))
     (wo-graph::copy-hash-table-slot result graph 'name-map)
     (wo-graph::copy-hash-table-slot result graph 'reverse-name-map)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creation of the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-error-generating-references (references)
  "This is an helper function which works around bugs in libgit2.
It seems that some tags not containing any message will generate an error.
This method takes a list of references (names like branches and tags) and
tries to lookup the corresponding commit. All references which generate an error
during lookup are removed from the result."
  (let ((result (loop :for reference :in references
		   :when (handler-case  (cl-git::commit-oid-from-oid (cl-git::lookup-oid :head reference))
#+nil			   (condition (x) (format t "Got error for: ~A and the error is: ~A~%" reference x) nil))
		   :collect reference)))
    result))


;;;;;;
(defun get-git-graph (git-dir)
  "Creates a graph based on the repository in the `git-dir'.
It will create this graph by starting with all references it can find and follow
the parents of those references recursively until it has build the whole graph.

All the names found by `cl-git:git-reference-listall' are used as starting point
and will be kept as names for later symbolic lookup with `name-to-vertex'.

In addition, for each commit which does not have a parent, a fake name of the form
`Initial-~D' is created.

The return value is of the type `git-graph'."
  (let ((graph (make-instance 'git-graph)))
    (cl-git:with-repository (git-dir)
      (cl-git:with-git-revisions
	  (commit :head (remove-error-generating-references
			 (cl-git:git-reference-listall :OID :PACKED)))
	(loop :for parent :in (cl-git::commit-parent-oids commit)
	   :do
	   (add-edge parent (cl-git::git-object-id commit)
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
a SHA1 for a different commit, the named vertex will be returned.

If the value is neither a valid name, nor a valid SHA1 it still returns a value, but
this value is not a vertex in the graph.  So error recovery from this function is
not easily done."
  (typecase name-or-rev
    (string
     (or (name-to-vertex name-or-rev git-graph)
	 (parse-integer name-or-rev :radix 16 :junk-allowed t)))
    (t nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun boundary-names (git-graph)
  "Returns names of vertices which are at a boundary, that is they
have eiter no parents or no children"
  (loop :for name :in (all-names git-graph)
     :for vertex = (name-to-vertex name git-graph)
     :unless (and (targets-of-vertex vertex git-graph) (sources-of-vertex vertex git-graph))
     :collect name))
