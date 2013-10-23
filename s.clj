(ns synacor-challenge
  (:import (java.io RandomAccessFile)))

(def registry (atom [0 0 0 0 0 0 0 0]))
(def stack (atom []))
(def instruction-pointer (atom 0))

(defn set-instruction-pointer
  [index]
  (swap! instruction-pointer index))

(defn get-instruction
  [index]
  (nth @program index))

(defn set-instruction
  [index value]
  (swap! program (assoc @program index instruction value)))

(defn from-little-endian
  [lo hi]
  (+ lo (bit-shift-left hi 8)))

(defn load-program
  [filename]
  (map (fn [[lo hi]] (from-little-endian lo hi)) (partition 2 (into-array Byte/TYPE (.getBytes (slurp filename))))))

(def program (atom (vec (load-program "/home/robert/synacor/challenge.bin"))))

(defn get-registry-value
  [register]
  (nth @registry register))

(defn set-registry-value
  [register value]
  (swap! registry (assoc @registry register value)))

(defn push-stack
  [value]
  (swap! stack (conj @stack value)))

(defn pop-stack
  []
  (let value (peek @stack)
    (swap! stack (pop @stack))
    value))

(defn get-value
  [value]
  (if (> value 32767)
    (get-registry-value (- value 32768))
    value))

(def opcodes [
  ;; 0 - halt
  (fn [] (System/exit 0))
  ;; 1 - set register
  (fn [a b] (set-registry-value (get-value a) (get-value b)))
  ;; 2 - push to stack
  (fn [a] (push-stack (get-value a)))
  ;; 3 - pop from stack to register
  (fn [a] (set-registry-value (get-value a) (pop-stack)))
  ;; 4 - eq 1 if b = c else 0
  (fn [a b c] (set-registry-value (get-value a) (if (= (get-value b) (get-value c)) 1 0)))
  ;; 5 - gt 1 if b > c else 0
  (fn [a b c] (set-registry-value (get-value a) (if (> (get-value b) (get-value c)) 1 0)))
  ;; 6 - jump to a
  (fn [a] (set-instruction-pointer (get-value a)))
  ;; 7 - if a != 0 jump to b
  (fn [a b] (if (not (= (get-value a) 0)) (set-instruction-pointer (get-value b))))
  ;; 8 - if a = 0 jump to b
  (fn [a b] (if (= (get-value a) 0) (set-instruction-pointer (get-value b))))
  ;; 9 - assign b + c (mod 32768) to a
  (fn [a b c] (set-registry-value (get-value a) (mod (+ (get-value b) (get-value c)) 32768)))
  ;; 10 - assign b * c (mod 32768) to a
  (fn [a b c] (set-registry-value (get-value a) (mod (* (get-value b) (get-value c)) 32768)))
  ;; 11 - assign b mod c to a
  (fn [a b c] (set-registry-value (get-value a) (mod (get-value b) (get-value c))))
  ;; 12 - assign b ^ c to a
  (fn [a b c] (set-registry-value (get-value a) (bit-and (get-value b) (get-value c))))
  ;; 13 - assign b | c to a
  (fn [a b c] (set-registry-value (get-value a) (bit-or (get-value b) (get-value c)) ))
  ;; 14 - assign ~b to a
  (fn [a b] (set-registry-value (get-value a) (bit-not (get-value b))))
  ;; 15 - write memory value at b to a
  (fn [a b] (set-registry-value (get-value a) (get-instruction (get-value b))))
  ;; 16 - write b to memory a
  (fn [a b] (set-instruction (get-value a) (get-value b)))
  ;; 17 - write next instruction to stack and jump to a
  (fn [a] (do (push-stack (+ @instruction-pointer 1)) (set-instruction-pointer (get-value a))))
  ;; 18 - pop stack and jump to instruction if exists else halt
  (fn [] (let (next-instruction (peek @stack)) (if (= next-instruction nil) (System/exit 0) (set-instruction-pointer next-instruction))))
  ;; 19 - write ascii a to terminal
  (fn [a] (print (char (get-value a))))
  ;; 20 - Read a character from stdin and store in a
  (fn [a] (set-registry-value (get-value a) (first (read-line))))
  ;; 21 - Noop
  (fn [] nil)
]





