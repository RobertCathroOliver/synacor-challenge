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

(def input-file "/home/robert/synacor/challenge.bin")

(defn get-register
  [value]
  (- value 32768))

(defn get-arity
  [f]
  (alength (.getParameterTypes (first (filter #(= "invoke" (.getName %)) (.getDeclaredMethods (class f)))))))

(defn create-program
  [instructions]
  {:instructions instructions :registry [0 0 0 0 0 0 0 0] :stack [] :instruction-ptr 0 :next-instruction-ptr nil :buffer "" :debug false :step false})

(defn toggle-debug
  [program]
  (assoc program :debug true))

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

(defn read-character
  [program]
  (let [buffer (if (empty? (program :buffer)) (str (read-line) \newline) (program :buffer))]
    (cond
      (= "~\n" buffer) (do (show-program-state program) (read-character program))
      (= "!\n" buffer) (assoc (set-instruction-pointer program nil) :buffer "\n")
      (= "#\n" buffer) (assoc (read-character program) :debug (not (program :debug)))
      (= \@ (first buffer)) (assoc program :registry (:registry (execute-program (create-program (string->ints (rest buffer))))) :buffer buffer)
      :else (assoc program :buffer buffer))))


(def opcodes [
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
        args (concat [(assoc program :next-instruction-ptr (+ 1 arity (program :instruction-ptr)))] (take arity (drop (+ index 1) (program :instructions))))
        result (apply op args)]
      (if (program :debug) (show-program-state program))
      result))

(defn run-program
  [program]
  (let [result (step-program program)]
    (if (not (= (result :next-instruction-ptr) nil)) (recur (assoc result :instruction-ptr (result :next-instruction-ptr))) result)))

(defn execute-program-old
  [program]
  (let [index (program :instruction-ptr)
        instruction (get-instruction program index)
        op (nth opcodes instruction)
        arity (- (get-arity op) 1)
        args (concat [(assoc program :next-instruction-ptr (+ 1 arity (program :instruction-ptr)))] (take arity (drop (+ index 1) (program :instructions))))
        result (apply op args)]
      (if (program :debug) (show-program-state program))
      (if (not (= (result :next-instruction-ptr) nil)) (recur (assoc result :instruction-ptr (result :next-instruction-ptr))) result)))

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
