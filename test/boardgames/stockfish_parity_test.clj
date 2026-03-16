(ns boardgames.stockfish-parity-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [boardgames.core :as core]
            [boardgames.chess :as chess]
            [boardgames.stockfish-test-utils :as sf]))

(use-fixtures :once sf/stockfish-fixture)

(def ^:private stockfish-chess-game
  (core/make-game "Chess"
                  chess/initiate-chess-board
                  chess/chess-expansion-rules
                  chess/chess-aggregate-rules))

(def ^:private parity-positions
  [{:name "Initial position (white to move)"
    :board chess/initial-chess-symbolic-board
    :turn 0
    :fen {:castling "KQkq"}}

   {:name "Initial position (black to move)"
    :board chess/initial-chess-symbolic-board
    :turn 1
    :fen {:castling "KQkq"}}

   {:name "Minimal kings + rook (no castling rights)"
    :board '[[- - - - k - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - K - - R]]
    :turn 0
    :fen {:castling "-"}}

   {:name "White short castling available"
    :board '[[- - - - k - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - K - - R]]
    :turn 0
    :fen {:castling "K"}}

   {:name "White short castling blocked by attack on path"
    :board '[[- - - - k r - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - K - - R]]
    :turn 0
    :fen {:castling "K"}}

   {:name "Knight mobility in open board"
    :board '[[- - - - k - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - N - - - -]
             [- - - - - - - -]
             [- - - - - - - -]
             [- - - - K - - -]]
    :turn 0
    :fen {:castling "-"}}])

(defn- assert-parity!
  [{:keys [name board turn fen]}]
  (testing name
    (let [game (cond-> (core/start-game stockfish-chess-game (core/symbolic->board board))
                 (= 1 turn) core/switch-turn)
          fen-str (sf/game->fen game fen)
          pmoves (core/possible-pmoves game)
          uci->pmove (into {} (map (fn [pmove] [(sf/pmove->uci pmove) pmove]) pmoves))
          engine-moves (set (keys uci->pmove))
          stockfish-result (try
                             {:moves (sf/stockfish-legal-uci-moves fen-str)}
                             (catch Throwable t
                               {:error t}))]
      (if-let [t (:error stockfish-result)]
        (is false {:name name
                   :fen fen-str
                   :board (:board game)
                   :error (str t)})
        (let [stockfish-moves (:moves stockfish-result)
            stockfish-boards (->> stockfish-moves
                      (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                      vec)
            engine-boards (->> engine-moves
                     (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                     vec)
              missing (set/difference stockfish-moves engine-moves)
              extra (set/difference engine-moves stockfish-moves)]
          (is (= (set stockfish-boards) (set engine-boards))
            (str "FEN: " fen-str
               "\nMissing (Stockfish only): " (pr-str (sort missing))
               "\nExtra (Engine only): " (pr-str (sort extra))))))

      ;; Return the same test-case payload shape used by other tests so Clerk can render boards.
      (let [stockfish-moves (or (:moves stockfish-result) #{})
            stockfish-boards (->> stockfish-moves
                                  (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                                  vec)
            engine-boards (->> engine-moves
                               (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                               vec)]
        ^{:boardgames/testcase true}
        [board stockfish-boards engine-boards]))))

(deftest ^:stockfish stockfish-legal-move-parity-test
  (if-not (sf/stockfish-available?)
    (is true (str "Skipping Stockfish parity tests: " (sf/stockfish-unavailable-reason)))
    (doall (map assert-parity! parity-positions))))

(deftest ^:stockfish stockfish-legal-move-parity-random
  (if-not (sf/stockfish-available?)
    (is true (str "Skipping random-position Stockfish parity tests: " (sf/stockfish-unavailable-reason)))
    (doall
     (for [game (sf/random-positions stockfish-chess-game :n-games 5 :max-plies 30)]
       (let [fen-str (sf/game->fen game)
             pmoves (core/possible-pmoves game)
             uci->pmove (into {} (map (fn [pmove] [(sf/pmove->uci pmove) pmove]) pmoves))
             engine-moves (set (keys uci->pmove))]
         (testing (str "FEN: " fen-str)
           (let [stockfish-result (try
                                    {:moves (sf/stockfish-legal-uci-moves fen-str)}
                                    (catch Throwable t
                                      {:error t}))]
             (if-let [t (:error stockfish-result)]
               (is false {:fen fen-str
                          :board (:board game)
                          :error (str t)})
               (let [stockfish-moves (:moves stockfish-result)
                    stockfish-boards (->> stockfish-moves
                                          (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                                          vec)
                    engine-boards (->> engine-moves
                                       (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                                       vec)
                     missing (set/difference stockfish-moves engine-moves)
                     extra (set/difference engine-moves stockfish-moves)]
                 (is (= (set stockfish-boards) (set engine-boards))
                     (str "FEN: " fen-str
                          "\nMissing (Stockfish only): " (pr-str (sort missing))
                          "\nExtra (Engine only): " (pr-str (sort extra))))))

             (let [stockfish-moves (or (:moves stockfish-result) #{})
                   stockfish-boards (->> stockfish-moves
                                         (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                                         vec)
                   engine-boards (->> engine-moves
                                      (keep #(some-> (get uci->pmove %) :steps first :board core/board->symbolic))
                                      vec)]
               ^{:boardgames/testcase true}
               [(core/board->symbolic (:board game)) stockfish-boards engine-boards]))))))))