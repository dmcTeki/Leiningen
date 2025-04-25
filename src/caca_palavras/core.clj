(ns caca-palavras.core
  (:require [clojure.string :as str]))

(def todas-palavras ["LIVRO" "CADERNO" "LAPIS" "BORRACHA" "MOCHILA" "TESOURA" "CANETA" "QUADRO" "GIZ"
                     "PAPEL" "FOLHA" "LIVRARIA" "ESCOLA" "PROFESSOR" "ALUNO" "AULA" "TURMA" "EXAME"
                     "NOTA" "TAREFA" "ATIVIDADE" "DICIONARIO"])

(def alfabeto "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def palavras-escondidas (atom [])) ; agora é um atom

(defn gerar-grade [tamanho]
  (vec (repeat tamanho (vec (repeat tamanho nil)))))

(defn pode-inserir? [grade palavra linha coluna delta-linha delta-coluna]
  (let [tamanho (count grade)
        posicoes (for [i (range (count palavra))]
                   [(+ linha (* i delta-linha)) (+ coluna (* i delta-coluna))])]
    (every? (fn [[l c]]
              (and (< l tamanho)
                   (< c tamanho)
                   (nil? (get-in grade [l c]))))
            posicoes)))

(defn inserir-em-grade [grade palavra linha coluna delta-linha delta-coluna]
  (let [posicoes (for [i (range (count palavra))]
                   [(+ linha (* i delta-linha)) (+ coluna (* i delta-coluna))])]
    (reduce (fn [g [[l c] ch]]
              (assoc-in g [l c] (str ch)))
            grade
            (map vector posicoes palavra))))

(defn inserir-horizontal [grade palavra linha coluna]
  (let [tamanho (count grade)]
    (if (<= (+ coluna (count palavra)) tamanho)
      (if (pode-inserir? grade palavra linha coluna 0 1)
        (inserir-em-grade grade palavra linha coluna 0 1)
        nil)
      nil)))

(defn inserir-vertical [grade palavra linha coluna]
  (let [tamanho (count grade)]
    (if (<= (+ linha (count palavra)) tamanho)
      (if (pode-inserir? grade palavra linha coluna 1 0)
        (inserir-em-grade grade palavra linha coluna 1 0)
        nil)
      nil)))

(defn inserir-palavra-na-grade [grade palavra]
  (let [palavra-up (str/upper-case palavra)
        tamanho (count grade)
        direcoes [:horizontal :vertical]]
    (loop [tentativas 0]
      (if (>= tentativas 100)
        (do
          (println "Não foi possível inserir a palavra:" palavra-up)
          nil)
        (let [linha (rand-int tamanho)
              coluna (rand-int tamanho)
              direcao (rand-nth direcoes)
              nova-grade
              (case direcao
                :horizontal (inserir-horizontal grade palavra-up linha coluna)
                :vertical (inserir-vertical grade palavra-up linha coluna))]
          (if nova-grade
            nova-grade
            (recur (inc tentativas))))))))

(defn preencher-grade [grade]
  (mapv (fn [linha]
          (mapv #(if (nil? %) (str (rand-nth alfabeto)) %) linha))
        grade))

(defn gerar-grade-com-palavras [tamanho palavras]
  (loop [grade (gerar-grade tamanho)
         palavras-a-inserir palavras
         palavras-inseridas []]
    (if (or (empty? palavras-a-inserir)
            (>= (count palavras-inseridas) 4))
      (do
        (reset! palavras-escondidas palavras-inseridas)
        (preencher-grade grade))
      (let [palavra (first palavras-a-inserir)
            nova-grade (inserir-palavra-na-grade grade palavra)]
        (if (nil? nova-grade)
          (recur grade (rest palavras-a-inserir) palavras-inseridas)
          (recur nova-grade (rest palavras-a-inserir) (conj palavras-inseridas palavra)))))))

(defn exibir-grade [grade]
  (doseq [linha grade]
    (println (str/join " " linha))))

(defn contem-substring-exata? [linha palavra]
  (let [linha-str (str/join "" linha)
        palavra-up (str/upper-case palavra)]
    (some #(= palavra-up %)
          (map #(subs linha-str % (+ % (count palavra-up)))
               (range (inc (- (count linha-str) (count palavra-up))))))))

(defn palavra-horizontal? [grade palavra]
  (some #(contem-substring-exata? % palavra) grade))

(defn palavra-vertical? [grade palavra]
  (let [transposta (apply map vector grade)]
    (some #(contem-substring-exata? % palavra) transposta)))

(defn encontrou-palavra? [grade palavra]
  (or (palavra-horizontal? grade palavra)
      (palavra-vertical? grade palavra))) ; por enquanto, só horizontal e vertical

(defn jogar []
  (let [tamanho 10
        grade (gerar-grade-com-palavras tamanho (shuffle todas-palavras))
        palavras-restantes (atom (set @palavras-escondidas))]
    (println "Bem-vindo ao jogo de Caça-Palavras!")
    (println "Tente encontrar todas as palavras escondidas na grade.")
    (println)
    (loop []
      (exibir-grade grade)
      (println)
      (print "Palavras restantes: " (count @palavras-restantes) " palavras\n")
      (println "Palavras: " (str/join ", " todas-palavras))
      (println)
      (print "\nDigite uma palavra encontrada: ")
      (flush)
      (let [entrada (read-line)
            palavra (str/upper-case (str/trim entrada))]
        (cond
          (and (contains? @palavras-restantes palavra)
               (encontrou-palavra? grade palavra))
          (do
            (swap! palavras-restantes disj palavra)
            (println (str "\nPalavra \"" palavra "\" encontrada!\n"))
            (if (empty? @palavras-restantes)
              (println "Parabéns! Você encontrou todas as palavras!")
              (recur)))
          :else
          (do
            (println "\nPalavra incorreta ou já encontrada. Tente novamente.\n")
            (recur)))))))

(defn -main []
  (jogar))
