;; Synacor challenge
;; Robert Cathro-Oliver
;; 1 November 2013

;; Notes:
;; Code 1: JCievrHAEENZ
;;   From the arch-spec
;; Code 2: stuFCvWbIDEl
;;   Get the VM running with opcode 19 (output)
;; Code 3: CYyTJnEGoxLF
;;   Run the self-test successfully
;; Code 4: zjmMIjVZgqWp
;;   'use tablet' to write this code on the tablet
;; Code 5: jXewMnpBsKyd
;;   Chiselled in the wall amidst winding passages.
;;   Use coins in order on slot machine to solve equation.
;;   red coin = 2, blue coin = 9, shiny coin = 5, concave coin = 7, corroded coin = 3
;;   (blue = 9) + (red = 2) * (shiny = 5) ^2 + (concave = 7) ^3 - (corroded = 3) = 399
;; Code 6: JZyMwNrDkeSD
;;   Use teleporter with register 8 = 0
;; Code 7: mpebObUrEhkJ
;;   Use teleporter with register 8 = 25734, register 0 = 6, skip instruction 5489
;;   Used find-instructions function to find where register 8 was being used in the code.
;;   Found the calculation at instruction 6027.  Found the test at 5391 and the call to 6027 at 5389.
;;   6027 describes a recursive function:
;;     (defn f [a b c]
;;       (cond
;;         (= 0 a) (+ b 1)
;;         (= 0 b) (f (- a 1) c c)
;;         :else (f (- a 1) (f a (- b 1) c) c)))
;;   Where initially a = register 1 = 4, b = register 2 = 1, c = register 8
;;   But this blows the stack.
;;   Unwind it a bit (by hand):
;;     (f 2 n c) == (+ (* c (+ n 2)) n 1)
;;     (f 3 n c) == (f 2 (f 3 (- n 1) c) c)
;;     (f 4 1 c) == (f 3 (f 3 c c) c)
;;   Write (f 3 b c) as tail recursive so as not to blow the stack
;;   Run this for all values of c (i.e. register 8) mod 32768 to find the value of 6 that gives a result of 6.
;; Code 8: YwMWoAup8Wbl
;;   Solve weight problem to get to vault
;;   Path = N E E N W S E E W N N E
;;   Take mirror and use mirror to get "ldW8quAoWMwY"
;;   Reverse string and take mirror-image of characters
;;   To solve the weight problem:
;;     Found a solution manually that took 14 steps
;;     Minimum solution is 6 steps
;;     All solutions have even length
;;     Given grid, start position, end position, length and final value, get-path function calculates all
;;       paths fitting the criteria
;;     (get-path grid [0 3] [3 0] 6 30) -> no results
;;     (get-path grid [0 3] [3 0] 8 30) -> no results
;;     (get-path grid [0 3] [3 0] 10 30) -> no results
;;     (get-path grid [0 3] [3 0] 12 30)

(ns synacor.core
  (:gen-class))

(import java.nio.file.Files)
(import java.nio.file.Paths)
(import java.nio.file.Path)
(import java.net.URI)

(defn from-little-endian
  [lo hi]
  (+ (bit-and lo 0xff) (bit-shift-left (bit-and hi 0xff) 8)))

(defn load-bytes
  [filename]
  (Files/readAllBytes (Paths/get (URI. (str "file://" filename)))))

(defn bytes->ints
  [bytes]
  (map (fn [[lo hi]] (from-little-endian lo hi)) (partition 2 bytes)))

(defn string->ints
  [string]
  (map #(Integer/valueOf %) (filter #(not (empty? %)) (clojure.string/split (apply str string) #"\s"))))

(defn get-register
  [value]
  (- value 32768))

(defn get-arity
  [f]
  (alength (.getParameterTypes (first (filter #(= "invoke" (.getName %)) (.getDeclaredMethods (class f)))))))

(defn create-program
  [instructions]
  {:instructions instructions :registry [0 0 0 0 0 0 0 0] :stack [] :instruction-ptr 0 :next-instruction-ptr nil :buffer ""})

(defn load-program
  [filename]
  (create-program (vec (bytes->ints (load-bytes filename)))))

(defn get-instruction
  [program index]
  (nth (program :instructions) index))

(defn set-instruction
  [program index instruction]
  (assoc program :instructions (assoc (program :instructions) index instruction)))

(defn set-instruction-pointer
  [program index]
  (assoc program :next-instruction-ptr index))

(defn get-registry-value
  [program register]
  (nth (program :registry) (get-register register)))

(defn get-value
  [program value]
  (if (> value 32767)
    (get-registry-value program value)
    value))

(defn set-registry-value
  [program register value]
  (assoc program :registry (assoc (program :registry) (get-register register) value)))

(defn push-stack
  [program value]
  (assoc program :stack (conj (program :stack) value)))

(defn pop-stack
  [program]
  (assoc program :stack (pop (program :stack))))

(declare opcodes)

(defn show-program-state
  [program]
  (let [index (program :instruction-ptr)
        instruction (get-instruction program index)
        op (nth opcodes instruction)
        arity (- (get-arity op) 1)
        args (concat [(assoc program :next-instruction-ptr (+ 1 arity (program :instruction-ptr)))] (take arity (drop (+ index 1) (program :instructions))))]
  (println index instruction (rest args) (program :registry) (program :stack) (program :buffer))))

(declare execute-program)

(defn set-buffer
  [program script]
  (assoc program :buffer script))

(defn read-character
  [program]
  (if (empty? (program :buffer)) (set-buffer program (str (read-line) \newline)) program))

(def ^:dynamic opcodes [
  ;; 0 - halt
  (fn [p] (set-instruction-pointer p nil))
  ;; 1 - set register
  (fn [p a b] (set-registry-value p a (get-value p b)))
  ;; 2 - push to stack
  (fn [p a] (push-stack p (get-value p a)))
  ;; 3 - pop from stack to register
  (fn [p a] (let [value (peek (p :stack))] (set-registry-value (pop-stack p) a value)))
  ;; 4 - eq 1 if b = c else 0
  (fn [p a b c] (set-registry-value p a (if (= (get-value p b) (get-value p c)) 1 0)))
  ;; 5 - gt 1 if b > c else 0
  (fn [p a b c] (set-registry-value p a (if (> (get-value p b) (get-value p c)) 1 0)))
  ;; 6 - jump to a
  (fn [p a] (set-instruction-pointer p (get-value p a)))
  ;; 7 - if a != 0 jump to b
  (fn [p a b] (if (not (= (get-value p a) 0)) (set-instruction-pointer p (get-value p b)) p))
  ;; 8 - if a = 0 jump to b
  (fn [p a b] (if (= (get-value p a) 0) (set-instruction-pointer p (get-value p b)) p))
  ;; 9 - assign b + c (mod 32768) to a
  (fn [p a b c] (set-registry-value p a (mod (+ (get-value p b) (get-value p c)) 32768)))
  ;; 10 - assign b * c (mod 32768) to a
  (fn [p a b c] (set-registry-value p a (mod (* (get-value p b) (get-value p c)) 32768)))
  ;; 11 - assign b mod c to a
  (fn [p a b c] (set-registry-value p a (mod (get-value p b) (get-value p c))))
  ;; 12 - assign b ^ c to a
  (fn [p a b c] (set-registry-value p a (bit-and (get-value p b) (get-value p c))))
  ;; 13 - assign b | c to a
  (fn [p a b c] (set-registry-value p a (bit-or (get-value p b) (get-value p c)) ))
  ;; 14 - assign ~b to a
  (fn [p a b] (set-registry-value p a (bit-and 0x7fff (bit-not (get-value p b)))))
  ;; 15 - write memory value at b to a
  (fn [p a b] (set-registry-value p a (get-instruction p (get-value p b))))
  ;; 16 - write b to memory a
  (fn [p a b] (set-instruction p (get-value p a) (get-value p b)))
  ;; 17 - write next instruction to stack and jump to a
  (fn [p a] (set-instruction-pointer (push-stack p (+ (p :instruction-ptr) 2)) (get-value p a)))
  ;; 18 - pop stack and jump to instruction if exists else halt
  (fn [p] (let [next-instruction (get-value p (peek (p :stack)))] (set-instruction-pointer (pop-stack p) next-instruction)))
  ;; 19 - write ascii a to terminal
  (fn [p a] (print (char (get-value p a))) (flush) p)
  ;; 20 - Read a character from stdin and store in a
  (fn [p a] (let [p2 (read-character p) character (int (first (p2 :buffer)))] (set-registry-value (assoc p2 :buffer (rest (p2 :buffer))) a character)))
  ;; 21 - Noop
  (fn [p] p)
])

(defn step-program
  [program]
  (let [index (program :instruction-ptr)
        instruction (get-instruction program index)
        op (nth opcodes instruction)
        arity (- (get-arity op) 1)
        args (concat [(assoc program :next-instruction-ptr (+ 1 arity (program :instruction-ptr)))] (take arity (drop (+ index 1) (program :instructions))))]
      (apply op args)))

(defn one-step
  [program]
  (let [result (step-program program)]
    (if (not (= (result :next-instruction-ptr) nil)) 
      (assoc result :instruction-ptr (result :next-instruction-ptr))
      nil)))

(defn step-to-buffer-drain
  [program]
  (if (and 
        (empty? (program :buffer))
        (= 20 (get-instruction program (program :instruction-ptr))))
    program
    (recur (one-step program))))
  
(defn step-to-match-output
  [program to-match]
  (binding [opcodes (assoc opcodes 19 (fn [p a] (let [c (char (get-value p a))] (print c) (flush) (assoc p :output (conj (p :output) c)))))]
    (loop [p program]
      (if (and (not (= p nil))
               (not (re-find (re-pattern to-match) (apply str (reverse (p :output))))))
        (recur (one-step p))
        (dissoc p :output)))))

(defn step-to-breakpoint
  [program breakpoints]
  (if (contains? breakpoints (program :instruction-ptr))
    program
    (recur (one-step program) breakpoints)))

(defn debug-program
  [program]
  (if (not (= program nil))
    (do
      (print "> ")
      (flush)
      (let [input (read-line)
            steps (re-find (re-matcher #"^\d+$" input))]
        (cond
          ;; Show the current program context
          (= "@" input) (do (show-program-state program) (flush) (recur program))
          ;; Stop debugging
          (= "!" input) program
          ;; Go until input required and buffer empty
          (= "+" input)
            (recur (step-to-buffer-drain program))
          ;; Step once
          (empty? input) 
            (let [p (one-step program)]
              (show-program-state program)
              (flush)
              (recur p))
          ;; Step until output is matched
          (= \- (first input))
            (recur (step-to-match-output program (apply str (rest input))))
          ;; Step until one of the given breakpoints is reached
          (= \^ (first input))
            (recur (step-to-breakpoint program (set (string->ints (rest input)))))
          ;; Goto given line
          (= \# (first input))
            (recur (assoc program :instruction-ptr (first (string->ints (rest input)))))
          ;; Set register
          (= \= (first input))
            (let [registry (program :registry)
                  values (string->ints (rest input))]
              (recur (assoc program :registry (assoc registry (first values) (second values)))))
          ;; step 'steps' times
          (not (= nil steps))
            (recur 
              (loop [n (Integer/valueOf steps) p program] 
                (if (and (> n 0) (not (= p nil)))
                  (recur (- n 1) (one-step p))
                  p)))
          ;; put stuff on the input queue
          :else (recur (set-buffer program (str input \newline))))))))

(defn run-program
  [program]
  (let [result (step-program program)]
    (if (not (= (result :next-instruction-ptr) nil)) (recur (assoc result :instruction-ptr (result :next-instruction-ptr))) result)))

;; functions for introspection
(defn find-instructions
  [program instruction]
  (map first
    (filter #(= (second %) instruction)
      (map-indexed vector (program :instructions)))))

(defn show-instructions
  [program index quantity]
  (map #(if (< 32767 % 32776) (symbol (str "r" (- % 32768))) %) (take quantity (drop index (program :instructions)))))

(defn find-string
  [program string]
  (let [subsequence (map int string)
        length (count subsequence)]
    (->>
      (partition length 1 (program :instructions))
      (map-indexed vector)
      (filter #(= (second %) subsequence))
      (map first))))

(defn find-output-string
  [program string]
  (let [subsequence (flatten (map (fn [x] [19 (int x)]) string))
        length (count subsequence)]
    (->>
      (partition length 1 (program :instructions))
      (map-indexed vector)
      (filter #(= (second %) subsequence))
      (map first))))

(defn modexp [base exponent modulus]
  (loop [b base e exponent acc 1]
    (if (> e 0)
      (if (= 1 (mod e 2))
        (recur (mod (* b b) modulus) (bit-shift-right e 1) (mod (* b acc) modulus))
        (recur (mod (* b b) modulus) (bit-shift-right e 1) acc))
      acc)))

;; Based on the code at 6027
;; This blows the stack
;; (defn f [a b c]
;;  (cond
;;    (= 0 a) (+ b 1)
;;    (= 0 b) (f (- a 1) c c)
;;    :else (f (- a 1) (f a (- b 1) c) c)))
;; Unwind it a bit:
;; (f 2 n c) == (+ (* c (+ n 2)) n 1)
;; (f 3 n c) == (f 2 (f 3 (- n 1) c) c)
;; Write it tail recursive so as not to blow the stack:
;; (f 4 1 c) == (f 3 (f 3 c c) c)
;; registry value = 25734

(defn f2 [x c] (mod (+ (* (+ x 2) c) x 1) 32768))
(defn f3 [b c]
  (loop [x c k 0]
    (if (= b k)
      (f2 x c)
      (recur (f2 x c) (+ k 1)))))

;; Solve the weight grid
;; Found a solution manually that took 14 steps
;; Minimum solution is 6 steps
;; (get-path grid [0 3] [3 0] 6 30) -> no results
;; (get-path grid [0 3] [3 0] 10 30) -> no results
;; (get-path grid [0 3] [3 0] 12 30)
;; (:N :E :E :N :W :S :E :E :W :N :N :E)

;; This is from clojure.contrib.  I just didn't want to include dependencies
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
               (cons (map first v-seqs)
                     (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(def grid
  [[ *  8  -  1]
   [ 4  * 11  *]
   [ +  4  - 18]
   [22  -  9  *]])

(def directions
  {:N [0 -1] :E [1 0] :S [0 1] :W [-1 0]})

(defn go-grid [pos dir]
  (vector (+ (first pos) (first (directions dir))) (+ (second pos) (second (directions dir)))))

(defn go-path [start [dir & path]]
   (if (= dir nil)
     start
     (recur (go-grid start dir) path)))

(defn grid-value [grid pos]
  (nth (nth grid (second pos) nil) (first pos) nil))

(defn calc-grid [grid value pos1 pos2]
  (let [operator (grid-value grid pos1)
        operand (grid-value grid pos2)]
    (if (or (= nil operator) (= nil operand))
      nil
      (operator value operand))))

(defn calc-path [grid value pos start-pos end-pos [dir1 dir2 & path]]
  (let [pos1 (go-grid pos dir1)
        pos2 (go-grid pos1 dir2)
        result (calc-grid grid value pos1 pos2)]
    (cond
      (= result nil) nil
      (= pos2 start-pos) nil
      (= 0 (count path))
        (if (= pos2 end-pos)
           result
           nil)
      (= pos2 end-pos) nil
      :else (recur grid result pos2 start-pos end-pos path))))

(defn show-path [index]
  (loop [i index p []]
    (if (= 0 i)
      (reverse p)
      (recur (/ (- i (mod i 4)) 4) (conj p (nth (keys directions) (mod i 4)))))))


(defn get-path [grid start-pos end-pos length end-value]
  (let [all-paths (apply cartesian-product (repeat length (vec (keys directions))))
        path-values (map #(calc-path grid (grid-value grid start-pos) start-pos start-pos end-pos %) all-paths)
        path-details (filter #(= end-value (second %)) (map-indexed vector path-values))]
    (show-path (first (first path-details)))))
