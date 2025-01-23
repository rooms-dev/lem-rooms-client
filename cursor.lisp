(uiop:define-package #:lem-rooms-client/cursor
  (:use #:cl
        #:lem-rooms-client/utils
        #:lem-rooms-client/editor)
  (:export #:delete-cursors-by-excluding-ids
           #:delete-cursor
           #:update-cursor))
(in-package #:lem-rooms-client/cursor)

(defvar *cursors* '())

(defclass cursor-overlay (lem-core::cursor-overlay)
  ((client-id :initarg :client-id :reader cursor-overlay-client-id)
   (user-name :initarg :user-name)
   (popup-message :initarg :popup-message
                  :initform nil
                  :accessor cursor-overlay-popup-message)
   (range-overlay :initarg :range-overlay
                  :initform nil
                  :accessor cursor-overlay-range-overlay)))

(defun make-cursor-overlay (&key point attribute client-id user-name range-overlay)
  (let ((cursor (make-instance 'cursor-overlay
                               :client-id client-id
                               :user-name user-name
                               :start (lem:copy-point point :left-inserting)
                               :end (lem:copy-point point :left-inserting)
                               :buffer (lem:point-buffer point)
                               :temporary nil
                               :fake t
                               :attribute attribute
                               :range-overlay range-overlay)))
    (push cursor *cursors*)
    cursor))

(defun delete-cursor-overlay (cursor-overlay)
  (lem:delete-point (lem:overlay-start cursor-overlay))
  (lem:delete-point (lem:overlay-end cursor-overlay))
  (alexandria:when-let (popup-message (cursor-overlay-popup-message cursor-overlay))
    (lem:delete-popup-message popup-message))
  (lem:delete-overlay cursor-overlay)
  (when (cursor-overlay-range-overlay cursor-overlay)
    (lem:delete-overlay (cursor-overlay-range-overlay cursor-overlay))))

(defun delete-cursors-by-excluding-ids (ids)
  (dolist (cursor *cursors*)
    (unless (member (cursor-overlay-client-id cursor) ids)
      (delete-cursor-overlay cursor)
      (setf *cursors* (delete cursor *cursors*))))
  (values))

(defun delete-cursor (id)
  (let ((cursor (find id *cursors* :key #'cursor-overlay-client-id)))
    (when cursor
      (delete-cursor-overlay cursor)
      (setf *cursors* (remove id *cursors* :key #'cursor-overlay-client-id))))
  (values))

(defun find-cursor (id)
  (find id *cursors* :key #'cursor-overlay-client-id))

(defun point-in-window-p (point window)
  (lem:with-point ((view-top (lem:window-view-point window))
                   (view-bottom (lem:window-view-point window)))
    (unless (lem:line-offset view-bottom (1- (lem:window-height window)))
      (lem:buffer-end view-bottom))
    (lem:point<= view-top
                 point
                 view-bottom)))

(defun make-range-overlay (buffer range color)
  (when range
    (destructuring-bind (start-pos end-pos) range
      (lem:with-point ((start (lem:buffer-point buffer))
                       (end (lem:buffer-point buffer)))
        (lem:move-to-position start start-pos)
        (lem:move-to-position end end-pos)
        (lem:make-overlay start
                          end
                          (lem:make-attribute :foreground "black"
                                              :background (lighten-color color :factor 0.9)))))))

(defun update-range-overlay (overlay range)
  (when range
    (destructuring-bind (start-pos end-pos) range
      (lem:move-to-position (lem:overlay-start overlay) start-pos)
      (lem:move-to-position (lem:overlay-end overlay) end-pos))))

(defun update-cursor (buffer id name color position range)
  (let ((cursor (find-cursor id)))
    (cond ((null cursor)
           (assert color)
           (let ((attribute (lem:make-attribute
                             :foreground (best-foreground-color color)
                             :background color)))
             (lem:with-point ((point (lem:buffer-point buffer)))
               (lem:move-to-position point position)
               (setf cursor (make-cursor-overlay :point point
                                                 :attribute attribute
                                                 :client-id id
                                                 :user-name name)))))
          (t
           (unless (eq (lem:overlay-buffer cursor) buffer)
             (delete-cursor-overlay cursor)
             (lem:with-point ((point (lem:buffer-point buffer)))
               (setf cursor
                     (make-cursor-overlay :point point
                                          :attribute (lem:overlay-attribute cursor)
                                          :client-id id
                                          :user-name name))))
           (lem:move-to-position (lem:overlay-start cursor)
                                 position)
           (lem:move-to-position (lem:overlay-end cursor)
                                 position)))

    (if range
        (if (cursor-overlay-range-overlay cursor)
            (update-range-overlay (cursor-overlay-range-overlay cursor) range)
            (setf (cursor-overlay-range-overlay cursor)
                  (make-range-overlay buffer range color)))
        (when (cursor-overlay-range-overlay cursor)
          (lem:delete-overlay (cursor-overlay-range-overlay cursor))
          (setf (cursor-overlay-range-overlay cursor) nil)))

    (alexandria:when-let (popup-message (cursor-overlay-popup-message cursor))
      (lem:delete-popup-message popup-message))

    (when (point-in-window-p (lem:overlay-start cursor) (lem:current-window))
      (lem:save-excursion
        (setf (lem:current-buffer) buffer)
        (lem:move-to-position (lem:current-point) position)
        (setf (cursor-overlay-popup-message cursor)
              (lem:display-popup-message (format nil " ~A " name)
                                         :timeout 1
                                         :style `(:gravity :cursor
                                                  :use-border nil
                                                  :background-color ,color
                                                  :offset-y -1
                                                  :cursor-invisible t))))))
  (values))
