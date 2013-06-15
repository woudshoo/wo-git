;;;; wo-git.lisp

(in-package #:wo-git)

;;; "wo-git" goes here. Hacks and glory await!



(defun git-names (repository &optional (name-filter (constantly t)))
  "Returns a hash table which maps commit oid to a list of names.
The precondition is that the git repository is already opened"
  (let ((result (make-hash-table :test #'equalp)))
    (loop :for reference :in (cl-git:list-objects 'cl-git:reference repository)
       :for reference-name = (cl-git:full-name reference)
       :for obj = (cl-git:resolve reference '(cl-git:commit))
       :do
       (when (and obj (funcall name-filter reference-name))
	 (push reference-name (gethash (cl-git:oid obj) result (list)))))
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
(defun get-git-graph (git-dir &optional (name-filter (constantly t)))
  "Creates a graph based on the repository in the `git-dir'.
It will create this graph by starting with all references it can find and follow
the parents of those references recursively until it has build the whole graph.

All the names found by `cl-git:git-reference-list' are used as starting point
and will be kept as names for later symbolic lookup with `name-to-vertex'.

In addition, for each commit which does not have a parent, a fake name of the form
`Initial-~D' is created.

The return value is of the type `git-graph'."
  (let ((graph (make-instance 'git-graph)))
    (cl-git:with-repository (repo git-dir)
      (loop :with walker = (cl-git:revision-walk 
			    (mapcar (lambda (x) (cl-git:resolve x '(cl-git:commit)))
				    (cl-git:list-objects 'cl-git:reference repo)))
	 :for commit = (cl-git:next-revision walker)
	 :until (null commit)
	 :do
	 (loop :for parent :in (cl-git:parents commit)
	    :do
	    (add-edge (cl-git:oid parent) (cl-git:oid commit) nil graph)))
      (setf (name-map graph) (git-names repo name-filter))
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


