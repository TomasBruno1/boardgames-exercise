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
          engine-moves (sf/game->uci-moves game)
          stockfish-moves (sf/stockfish-legal-uci-moves fen-str)
          missing (set/difference stockfish-moves engine-moves)
          extra (set/difference engine-moves stockfish-moves)]

      (is (= stockfish-moves engine-moves)
          (str "FEN: " fen-str
               "\nMissing (Stockfish only): " (pr-str (sort missing))
               "\nExtra (Engine only): " (pr-str (sort extra)))))))

(deftest ^:stockfish stockfish-legal-move-parity-test
  (if-not (sf/stockfish-available?)
    (is true (str "Skipping Stockfish parity tests: " (sf/stockfish-unavailable-reason)))
    (doseq [position parity-positions]
      (assert-parity! position))))

(deftest ^:stockfish stockfish-legal-move-parity-random
  (if-not (sf/stockfish-available?)
    (is true (str "Skipping random-position Stockfish parity tests: " (sf/stockfish-unavailable-reason)))
    (doseq [game (sf/random-positions stockfish-chess-game :n-games 5 :max-plies 30)]
      (let [fen-str (sf/game->fen game)]
        (testing (str "FEN: " fen-str)
          (let [engine-moves    (sf/game->uci-moves game)
                stockfish-moves (sf/stockfish-legal-uci-moves fen-str)
                missing         (set/difference stockfish-moves engine-moves)
                extra           (set/difference engine-moves stockfish-moves)]
            (is (= stockfish-moves engine-moves)
                (str "Missing (Stockfish only): " (pr-str (sort missing))
                     "\nExtra (Engine only): " (pr-str (sort extra))))))))))