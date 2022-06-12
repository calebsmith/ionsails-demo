(ns ionsails.util.noise.perlin)


;N.B. - Many ideas a snippets taken from https://github.com/indy/perlin/,
; and https://github.com/josephg/noisejs/blob/master/perlin.js,
; and https://github.com/caseman/noise/blob/master/perlin.py
; This implementation is very flavored by these, but otherwise original.


; Permutation table is defined twice to allow for indexing to wrap-around
(def p [
        151 160 137 91 90 15
        131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23
        190  6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33
        88 237 149 56 87 174 20 125 136 171 168  68 175 74 165 71 134 139 48 27 166
        77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244
        102 143 54  65 25 63 161  1 216 80 73 209 76 132 187 208  89 18 169 200 196
        135 130 116 188 159 86 164 100 109 198 173 186  3 64 52 217 226 250 124 123
        5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42
        223 183 170 213 119 248 152  2 44 154 163  70 221 153 101 155 167  43 172 9
        129 22 39 253  19 98 108 110 79 113 224 232 178 185  112 104 218 246 97 228
        251 34 242 193 238 210 144 12 191 179 162 241  81 51 145 235 249 14 239 107
        49 192 214  31 181 199 106 157 184  84 204 176 115 121 50 45 127  4 150 254
        138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180

        151 160 137 91 90 15
        131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23
        190  6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33
        88 237 149 56 87 174 20 125 136 171 168  68 175 74 165 71 134 139 48 27 166
        77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244
        102 143 54  65 25 63 161  1 216 80 73 209 76 132 187 208  89 18 169 200 196
        135 130 116 188 159 86 164 100 109 198 173 186  3 64 52 217 226 250 124 123
        5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42
        223 183 170 213 119 248 152  2 44 154 163  70 221 153 101 155 167  43 172 9
        129 22 39 253  19 98 108 110 79 113 224 232 178 185  112 104 218 246 97 228
        251 34 242 193 238 210 144 12 191 179 162 241  81 51 145 235 249 14 239 107
        49 192 214  31 181 199 106 157 184  84 204 176 115 121 50 45 127  4 150 254
        138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180])


(def G1 [-1 1])

(def G2 [[1 0] [-1 0]
         [0 1] [0 -1]])

(def G3 [[ 1 1 0 ] [ -1 1 0 ] [ 1 -1 0 ] [ -1 -1 0 ]
         [ 1 0 1 ] [ -1 0 1 ] [ 1 0 -1 ] [ -1 0 -1 ]
         [ 0 1 1 ] [ 0 -1 1 ] [ 0 1 -1 ] [ 0 -1 -1 ]
         [ 1 1 0 ] [ -1 1 0 ] [ 0 -1 1 ] [ 0 -1 -1 ]])


(def G4 [[ -1 -1 -1 0 ] [ -1 -1 1 0 ] [ -1 1 -1 0 ] [ -1 1 1 0 ]
         [ 1 -1 -1 0 ] [ 1 -1 1 0 ] [ 1 1 -1 0 ] [ 1 1 1 0 ]
         [ -1 -1 0 -1 ] [ -1 1 0 -1 ] [ 1 -1 0 -1 ] [ 1 1 0 -1 ]
         [ -1 -1 0 1 ] [ -1 1 0 1 ] [ 1 -1 0 1 ] [ 1 1 0 1 ]
         [ -1 0 -1 -1 ] [ 1 0 -1 -1 ] [ -1 0 -1 1 ] [ 1 0 -1 1 ]
         [ -1 0 1 -1 ] [ 1 0 1 -1 ] [ -1 0 1 1 ] [ 1 0 1 1 ]
         [ 0 -1 -1 -1 ] [ 0 -1 -1 1 ] [ 0 -1 1 -1 ] [ 0 -1 1 1 ]
         [ 0 1 -1 -1 ] [ 0 1 -1 1 ] [ 0 1 1 -1 ] [ 0 1 1 1 ]])


(defn fade [t]
  (* t t t (+ (* t (- (* t 6) 15.0)) 10.0)))

(defn lerp [t a b]
  (+ a (* t (- b a))))

(defn cos-erp [x a b]
  (let [ft (* x Math/PI)
        f (* (- 1 (Math/cos ft)) 0.5)]
    (+ (* a (- 1 f)) (* b f))))

(defn grad1 [hash-val x]
  (let [h (bit-and hash-val 1)
        Gx (G1 h)]
    (* x Gx)))


(defn grad2 [hash-val x y]
  (let [h (bit-and hash-val 3)
        Gx ((G2 h) 0)
        Gy ((G2 h) 1)]
    (+
     (* x Gx)
     (* y Gy))))

(defn grad3
  [hash-val x y z]
  (let [xn (- x)
        yn (- y)
        zn (- z)]
    (condp = (bit-and hash-val 15)
      0 (+ x y)
      1 (+ xn y)
      2 (+ x yn)
      3 (+ xn yn)
      4 (+ x z)
      5 (+ xn z)
      6 (+ x zn)
      7 (+ xn zn)
      8 (+ y z)
      9 (+ yn z)
      10 (+ y zn)
      11 (+ yn zn)
      12 (+ x y)
      13 (+ yn z)
      14 (+ xn y)
      15 (+ yn zn))))

(defn grad4 [hash-val x y z w]
  (let [h (bit-and hash-val 31)
        Gx ((G4 h) 0)
        Gy ((G4 h) 1)
        Gz ((G4 h) 2)
        Gw ((G4 h) 3)]
    (+
     (* x Gx)
     (* y Gy)
     (* z Gz)
     (* w Gw))))

(defn raw-perlin1 [x]
  (let [X (bit-and (int x) 255)
        xx (- x (int x))
        u (fade xx)
        A (p X)
        B (p (+ X 1))]
    (lerp u (grad1 (p A) xx) (grad1 (p B) (dec xx)))))


(defn raw-perlin2 [x y]
  (let [X (bit-and (int x) 255)
        Y (bit-and (int y) 255)
        xx (- x (int x))
        yy (- y (int y))
        u (fade xx)
        v (fade yy)
        A (+ (p X) Y)
        B (+ (p (+ X 1)) Y)]
    (lerp v
          (lerp u
                (grad2 (p A) xx yy)
                (grad2 (p B) (- xx 1) yy))
          (lerp u
                (grad2 (p (+ 1 A)) xx (- yy 1))
                (grad2 (p (+ 1 B)) (- xx 1) (- yy 1))))))

(defn raw-perlin3 [x y z]
  (let [X (bit-and (int x) 255)
        Y (bit-and (int y) 255)
        Z (bit-and (int z) 255)
        xx (- x (int x))
        yy (- y (int y))
        zz (- z (int z))
        u (fade xx)
        v (fade yy)
        w (fade zz)
        A (+ (p X) Y)
        AA (+ (p A) Z)
        AB (+ (p (+ A 1)) Z)
        B (+ (p (+ X 1)) Y)
        BA (+ (p B) Z)
        BB (+ (p (+ B 1)) Z)]
    (lerp w
          (lerp v
                (lerp u
                      (grad3 (p AA) xx yy zz)
                      (grad3 (p BA) (- xx 1) yy zz))
                (lerp u
                      (grad3 (p AB) xx (- yy 1) zz)
                      (grad3 (p BB) (- xx 1) (- yy 1) zz)))
          (lerp v
                (lerp u
                      (grad3 (p (+ AA 1)) xx yy (- zz 1))
                      (grad3 (p (+ BA 1)) (- xx 1) yy (- zz 1)))
                (lerp u
                      (grad3 (p (+ AB 1)) xx (- yy 1) (- zz 1))
                      (grad3 (p (+ BB 1)) (- xx 1) (- yy 1) (- zz 1)))))))


(defn raw-perlin4 [x y z w]
  (let [l lerp
        X (bit-and (int x) 255)
        Y (bit-and (int y) 255)
        Z (bit-and (int z) 255)
        W (bit-and (int w) 255)
        xx (- x (int x))
        yy (- y (int y))
        zz (- z (int z))
        ww (- w (int w))
        u (fade xx)
        v (fade yy)
        t (fade zz)
        s (fade ww)
        A (+ (p X) Y)
        AA (+ (p A) Z)
        AB (+ (p (+ A 1)) Z)
        B (+ (p (+ X 1)) Y)
        BA (+ (p B) Z)
        BB (+ (p (+ B 1)) Z)
        AAA (+ (p AA) W)
        AAB (+ (p (+ AA 1)) W)
        ABA (+ (p AB) W)
        ABB (+ (p (+ AB 1)) W)
        BAA (+ (p BA) W)
        BAB (+ (p (+ BA 1)) W)
        BBA (+ (p BB) W)
        BBB (+ (p (+ BB 1)) W)]
    (l s
       (l t
          (l v
             (l u
                (grad4 (p AAA) xx yy zz ww)
                (grad4 (p BAA) (dec xx) yy zz ww))
             (l u
                (grad4 (p ABA) xx (dec yy) zz ww)
                (grad4 (p BBA) (dec xx) (dec yy) zz ww)))
          (l v
             (l u
                (grad4 (p AAB) xx yy (dec zz) ww)
                (grad4 (p BAB) (dec xx) yy (dec zz) ww))
             (l u
                (grad4 (p ABB) xx (dec yy) (dec zz) ww)
                (grad4 (p BBB) (dec xx) (dec yy) (dec zz) ww))))
       (l t
          (l v
             (l u
                (grad4 (p (inc AAA)) xx yy zz (dec ww))
                (grad4 (p (inc BAA)) (dec xx) yy zz (dec ww)))
             (l u
                (grad4 (p (inc ABA)) xx (dec yy) zz (dec ww))
                (grad4 (p (inc BBA)) (dec xx) (dec yy) zz (dec ww))))
          (l v
             (l u
                (grad4 (p (inc AAB)) xx yy (dec zz) (dec ww))
                (grad4 (p (inc BAB)) (dec xx) yy (dec zz) (dec ww)))
             (l u
                (grad4 (p (inc ABB)) xx (dec yy) (dec zz) (dec ww))
                (grad4 (p (inc BBB)) (dec xx) (dec yy) (dec zz) (dec ww))))))))

(defn raw-perlin
  "Givex 1 to 4 arguments, call the appropriate perlin noise function for that number
  of dimensions. These are 'raw' noise values that aren't sampled against a given rate
  or scaled to fill an expected range"
  ([x] (raw-perlin1 x))
  ([x y] (raw-perlin2 x y))
  ([x y z] (raw-perlin3 x y z))
  ([x y z w] (raw-perlin4 x y z w)))


(defn rated-args [rate args]
  (map #(/ % rate) args))

(defn rated-perlin
  "Samples perlin noise functions against a given sample rate. This is used to avoid
  the problem of sampling perlin with integer values, which always returns 0"
  [rate & args]
  (apply raw-perlin (rated-args rate args)))


(def perlin-range-scale
  "Adjusts the scale of raw perlin noise return values so the range is -1 to 1
  for all dimensions. Each value in this vector corresponds with perlin in 1 dimension,
  2 dimensions, and so on up to 4."
  [
   2
   (Math/sqrt 2)
   (Math/sqrt 3)
   1])


(defn clamp [minimum maximum value]
  "Given a minimum and maximum, returns a given value that doesn't exceed these bounds"
  (if (> value maximum)
    maximum
    (if (< value minimum)
      minimum
      value)))

(def clamp-1
  "A clamp for scaled perlin noise functions, which have a [-1, 1] range"
  (partial clamp -1.0 1.0))

(defn scaled-perlin [& args]
  (let [scale (perlin-range-scale (- (count args) 1))]
    (clamp-1 (* scale (apply raw-perlin args)))))

(defn perlin
  "Takes a rate for sampling rate, and 1 to 4 additional arguments and returns the perlin noise
  function called with that sample rate, and scaled for the number of dimensions"
  [rate & args]
  (apply scaled-perlin (rated-args rate args)))

(defn power-series
  "Given a base, returns the lazy sequence of powers from 0"
  [base]
  (map #(Math/pow base %) (range)))

(defn octave-perlin
  "Given a sample rate, number of octaves, a 'persistance' and 1 to 4 arguments
  for each dimenion, returns a sampled/scaled perlin noise with a sample for each 'octave',
  each applied to the previous depending on the amount of 'persistance'"
  [octaves persistance rate & args]
  (let [freqs (take octaves (power-series 2))
        amps (take octaves (power-series persistance))
        freqs-amps (map vector freqs amps)
        total (reduce +
                      (map (fn [[freq amp]]
                             (let [freqed-args (map #(* % freq) args)]
                               (* amp (apply perlin (cons rate freqed-args)))))
                           freqs-amps))
        maxval (reduce + amps)]
    (/ total maxval)))
