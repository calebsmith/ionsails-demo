(defproject ionsails-web "0.1.0-SNAPSHOT"
  :description "Ionsails"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [;; UI
                 [thheller/shadow-cljs "2.19.5"]
                  [reagent "1.1.1"]
                  ;; Data/algorithm libs
                  [amalloy/ring-buffer "1.2"]
                  [datascript "1.3.13"]
                  ;; Logic
                  [org.clojure/core.match "1.0.0"]
                  [instaparse "1.4.12"]
                  ;; Dev
                  [binaryage/devtools "1.0.6"]
                 ]

  :profiles {:dev {:source-paths ["src" "dev"]}}
  :source-paths ["src"]
  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"])
