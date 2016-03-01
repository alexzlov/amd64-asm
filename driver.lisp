;;; driver.lisp
;;; Driver for assembler

(in-package "AMD64-ASM")

(defun assemble (source)
  (let ((obj (new-asmobj)))
    (iter (for def in source)
      (ecase (first def)
        (:proc (emit-code-def obj
                  (third def)
                  (second def)
                  (assemble-code (cdddr def))))
        (:var (emit-data-def obj
                 (third def)
                 (second def)
                 (assemble-data (cdddr def))))))
    obj))

(defun assemble-and-output (source type filename)
  (let* ((obj (assemble source))
     (buf (ecase type
        (:mach-o (generate-mach-o-obj obj)))))
    (store-vector-into-file buf filename)
    filename))

(deftype vector4 () '(simple-array single-float (4)))
(deftype simple-ub8-vector () '(simple-array (unsigned-byte 8) (*)))

(defmacro with-pinned-vectors ((&rest specs) &body body)
  (if (null specs)
      `(progn ,@body)
      `(cffi:with-pointer-to-vector-data ,(first specs)
         (with-pinned-vectors ,(rest specs) ,@body))))

;;; Example below
(defvar *function* (coerce
                    #(#x8B #x44 #x24 #x08
                      #x0F #x10 #x00
                      #x8B #x44 #x24 #x0C
                      #x0F #x10 #x08
                      #x0F #x58 #xC1
                      #x8B #x44 #x24 #x04
                      #x0F #x11 #x00
                      #xC3)
                    'simple-ub8-vector))

(defun vector4-add (out v1 v2)
  (declare (type vector4 out v1 v2))
  (with-pinned-vectors ((function *function*)
                        (p-out out)
                        (p-v1  v1)
                        (p-v2  v2))
    (cffi:foreign-funcall-pointer
     function (:convention :cdecl)
     :pointer p-out
     :pointer p-v1
     :pointer p-v2
     :void))
  out)
