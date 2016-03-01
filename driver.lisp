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
;; (defvar *function* (coerce
;;                     #(#x8B #x44 #x24 #x08
;;                       #x0F #x10 #x00
;;                       #x8B #x44 #x24 #x0C
;;                       #x0F #x10 #x08
;;                       #x0F #x58 #xC1
;;                       #x8B #x44 #x24 #x04
;;                       #x0F #x11 #x00
;;                       #xC3)
;;                     'simple-ub8-vector))

;; (defun vector4-add (out v1 v2)
;;   (declare (type vector4 out v1 v2))
;;   (with-pinned-vectors ((function *function*)
;;                         (p-out out)
;;                         (p-v1  v1)
;;                         (p-v2  v2))
;;     (cffi:foreign-funcall-pointer
;;      function (:convention :cdecl)
;;      :pointer p-out
;;      :pointer p-v1
;;      :pointer p-v2
;;      :void))
;;   out)

;; (defparameter *out*
;;   (make-array 4 :element-type 'single-float
;;               :initial-contents '(.0 .0 .0 .0)))

;; (defparameter *v1*
;;   (make-array 4 :element-type 'single-float
;;               :initial-contents '(1.0 2.0 3.0 4.0)))

;; (defparameter *v2*
;;   (make-array 4 :element-type 'single-float
;;               :initial-contents '(5.0 6.0 7.0 8.0)))

;(vector4-add *out* *v1* *v2*)

;;; Another example
;; (defun asm-sum (list)
;;   (let ((program
;;          '(
;;            #x55                    ; push ebp
;;            #x8b #xec               ; mov  ebp, esp
;;            #x53                    ; push ebx
;;            #x51                    ; push ecx
;;            #x8b #x5d #x08          ; mov  ebx, DWORD PTR [ebp+4]
;;            #x8b #x4d #x0c          ; mov  ecx, DWORD PTR [ebp+8]
;;            #x33 #xc0               ; xor  eax, eax
;;            #x13 #x03               ; sum: adc  eax, DWORD PTR [ebx]
;;            #x83 #xc3 #x04          ; add  ebx, 4
;;            #xe2 #xf9               ; loop sum
;;            #x59                    ; pop  ecx
;;            #x5b                    ; pop  ebx
;;            #x5d                    ; pop  ebp
;;            #xc3                    ; ret
;;            )))
;;     (cffi:with-foreign-object (code :unsigned-char (length program))
;;       (loop for byte in program
;;          for i = 0 then (1+ i) do
;;            (setf (cffi:mem-aref code :unsigned-char i) byte))
;;       (cffi:with-foreign-object (data :int (length list))
;;         (loop for int in list
;;            for i = 0 then (1+ i) do
;;              (setf (cffi:mem-aref data :int i) int))
;;         (cffi:foreign-funcall code :pointer data :int (length list) :int)))))

;;(asm-sum '(20 -40 62))
;;(let ((pointer
;;       (cffi:foreign-alloc :unsigned-char
;;                           :initial-contents (amd64-asm::asmbin-buffer
;;                                              (amd64-asm::assemble-code
;;                                               '((:mov :rax 12)
;;                                                 (:mov :rbx 3)
;;                                                 (:add :rax :rbx)
;;                                                 (:ret)))))))
;;  (cffi:foreign-funcall-pointer pointer () :int))
