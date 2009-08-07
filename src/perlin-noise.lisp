;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; perlin-noise.lisp
;;;;
;;;;  author: Erik Winkels (aerique@xs4all.nl)
;;;; created: 2009-05-18
;;;; version: 0.1
;;;;
;;;; There's another implementation here:
;;; http://www.koders.com/lisp/fid49FCA421DBCE60AFE806BBCD84887C14ECD487D5.aspx
;;;;
;;;; It looks more extensive, but I just wanted to write my own to understand
;;;; it better.

(in-package :okra)


;;; Variables

(defparameter +perlin-noise-permutation+
  #(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142
    8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203
    117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74
    165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220
    105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132
    187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3
    64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227
    47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221
    153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185
    112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51
    145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121
    50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78
    66 215 61 156 180
    151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142
    8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203
    117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74
    165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220
    105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132
    187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3
    64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227
    47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221
    153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185
    112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51
    145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121
    50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78
    66 215 61 156 180))


;;; Functions

(defun fade (v)
  (* v v v (+ (* v (- (* v 6) 15)) 10)))


(defun grad (hash x y z)
  (declare (type fixnum hash))
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (= (logand h 1) 0) u (- u))
       (if (= (logand h 2) 0) v (- v)))))


(defun lerp (v a b)
  (+ a (* v (- b a))))


(defun perlin-noise (x y z)
  "X, Y and Z need to be numbers within -1.0 and 1.0."
  (declare (inline fade grad lerp)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3)))
  (let* ((xc (logand (floor x) 255))
         (yc (logand (floor y) 255))
         (zc (logand (floor z) 255))
         (x (- x (floor x)))
         (y (- y (floor y)))
         (z (- z (floor z)))
         (u (fade x))
         (v (fade y))
         (w (fade z))
         (a  (+ (svref +perlin-noise-permutation+    xc   ) yc))
         (aa (+ (svref +perlin-noise-permutation+    a    ) zc))
         (ab (+ (svref +perlin-noise-permutation+ (+ a  1)) zc))
         (b  (+ (svref +perlin-noise-permutation+ (+ xc 1)) yc))
         (ba (+ (svref +perlin-noise-permutation+    b    ) zc))
         (bb (+ (svref +perlin-noise-permutation+ (+ b  1)) zc)))
    (lerp w (lerp v (lerp u (grad    aa       x       y       z  )
                            (grad    ba    (- x 1)    y       z  ))
                    (lerp u (grad    ab       x    (- y 1)    z  )
                            (grad    bb    (- x 1) (- y 1)    z  )))
            (lerp v (lerp u (grad (+ aa 1)    x       y    (- z 1))
                            (grad (+ ba 1) (- x 1)    y    (- z 1)))
                    (lerp u (grad (+ ab 1)    x    (- y 1) (- z 1))
                            (grad (+ bb 1) (- x 1) (- y 1) (- z 1)))))))


(defun pn-bench (&key (fn 'perlin-noise) (iterations 1000000))
  (time (loop repeat iterations
              for x from 0.0 by 0.1
              for y from 0.0 by 0.01
              for z from 0.0 by 0.001
              do (funcall fn x y z))))


;;; Single-Float Version

;; f(t) = 6t^5 - 15t^4 + 10t3
(defun fade-sf (v)
  (declare (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float v))
  (* v v v (+ (* v (- (* v 6) 15)) 10)))


(defun grad-sf (hash x y z)
  (declare (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type fixnum hash)
           (type single-float x y z))
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (= (logand h 1) 0) u (- u))
       (if (= (logand h 2) 0) v (- v)))))


(defun lerp-sf (v a b)
  (declare (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float v a b))
  (+ a (* v (- b a))))


;; http://mrl.nyu.edu/~perlin/noise/
(defun perlin-noise-single-float (x y z)
  "X, Y and Z need to be SINGLE-FLOATS within -1.0 and 1.0."
  (declare (inline fade-sf grad-sf lerp-sf)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float x y z))
  (let* ((xc (logand (the fixnum (floor x)) 255))
         (yc (logand (the fixnum (floor y)) 255))
         (zc (logand (the fixnum (floor z)) 255))
         (x (- x (floor x)))
         (y (- y (floor y)))
         (z (- z (floor z)))
         (u (fade-sf x))
         (v (fade-sf y))
         (w (fade-sf z))
         (a  (+ (the fixnum (svref +perlin-noise-permutation+    xc   )) yc))
         (aa (+ (the fixnum (svref +perlin-noise-permutation+    a    )) zc))
         (ab (+ (the fixnum (svref +perlin-noise-permutation+ (+ a  1))) zc))
         (b  (+ (the fixnum (svref +perlin-noise-permutation+ (+ xc 1))) yc))
         (ba (+ (the fixnum (svref +perlin-noise-permutation+    b    )) zc))
         (bb (+ (the fixnum (svref +perlin-noise-permutation+ (+ b  1))) zc)))
    (lerp-sf w
      (lerp-sf v (lerp-sf u (grad-sf    aa       x       y       z  )
                            (grad-sf    ba    (- x 1)    y       z  ))
                 (lerp-sf u (grad-sf    ab       x    (- y 1)    z  )
                            (grad-sf    bb    (- x 1) (- y 1)    z  )))
      (lerp-sf v (lerp-sf u (grad-sf (+ aa 1)    x       y    (- z 1))
                            (grad-sf (+ ba 1) (- x 1)    y    (- z 1)))
                 (lerp-sf u (grad-sf (+ ab 1)    x    (- y 1) (- z 1))
                            (grad-sf (+ bb 1) (- x 1) (- y 1) (- z 1)))))))


;;; Faster but inaccurate version.
;;;
;;; see: http://www.gamedev.net/community/forums/mod/journal/journal.asp?jn=263350&reply_id=2889484

(defparameter +pn-fade-table+
  (loop for i from 0 to 255
        collect (the single-float (fade (- (/ i 127.5) 1.0))) into table
        finally (return (coerce table 'vector))))


(defun perlin-noise-faster-inaccurate (x y z)
  "X, Y and Z need to be numbers within -1.0 and 1.0."
  (declare (inline fade grad lerp)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3)))
  (let* ((xc (logand (floor x) 255))
         (yc (logand (floor y) 255))
         (zc (logand (floor z) 255))
         (x (- x (floor x)))
         (y (- y (floor y)))
         (z (- z (floor z)))
         (u (fade x))
         (v (fade y))
         (w (fade z))
         (a  (+ (svref +perlin-noise-permutation+    xc   ) yc))
         (aa (+ (svref +perlin-noise-permutation+    a    ) zc))
         (ab (+ (svref +perlin-noise-permutation+ (+ a  1)) zc))
         (b  (+ (svref +perlin-noise-permutation+ (+ xc 1)) yc))
         (ba (+ (svref +perlin-noise-permutation+    b    ) zc))
         (bb (+ (svref +perlin-noise-permutation+ (+ b  1)) zc)))
    (lerp w (lerp v (lerp u (grad    aa       x       y       z  )
                            (grad    ba    (- x 1)    y       z  ))
                    (lerp u (grad    ab       x    (- y 1)    z  )
                            (grad    bb    (- x 1) (- y 1)    z  )))
            (lerp v (lerp u (grad (+ aa 1)    x       y    (- z 1))
                            (grad (+ ba 1) (- x 1)    y    (- z 1)))
                    (lerp u (grad (+ ab 1)    x    (- y 1) (- z 1))
                            (grad (+ bb 1) (- x 1) (- y 1) (- z 1)))))))


;;; Fractal Brownian Motion

;(defun fbm (x y z &key (fn 'perlin-noise) (octaves 8) (multiplier 2))
;  (loop with result = 0.0
;        with scale = (/ 1.0 multiplier)
;        with weight = 1.0
;        repeat octaves
;        do (incf result (* (funcall fn (* x scale) (* y scale) (* z scale))
;                           weight))
;           (setf scale (* scale multiplier))
;           (setf weight (/ weight multiplier))
;        finally (return result)))
(defun fbm (x y z &key (fn 'perlin-noise) (octaves 8) (multiplier 2))
  (loop with result = 0.0
        with scale = (/ 1.0 multiplier)
        with weight = 1.0
        repeat octaves
        do (incf result (* (funcall fn (* x scale) (* y scale) (* z scale))
                           weight))
           (setf scale (* scale multiplier))
           (setf weight (/ weight multiplier))
        finally (return result)))


;; requires ZPNG
;(defun test-fbm (&key (width 512) (height 512) (fn 'perlin-noise))
;  (labels ((set-blue (image x y value)
;             (setf (aref image y x 2) value))
;           (set-green (image x y value)
;             (setf (aref image y x 1) value))
;           (set-red (image x y value)
;             (setf (aref image y x 0) value))
;           (set-rgb (image x y red green blue)
;             (set-blue image x y blue)
;             (set-green image x y green)
;             (set-red image x y red)))
;    (let* ((hh (/ height 2.0))
;           (hw (/ width 2.0))
;           (png (make-instance 'zpng:png :color-type :truecolor
;                               :width width :height height))
;           (image (zpng:data-array png)))
;      (time (with-xy (x width y height)
;              (let ((noise (floor (+ (* (fbm (/ (- x hw) hw) (/ (- y hh) hh)
;                                             0.0 :fn fn)
;                                        127)
;                                     127))))
;                (set-rgb image x y noise noise noise))))
;      (zpng:write-png png "tmp.png"))))
