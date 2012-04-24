(ns blog)

(defprotocol Publishable
  (as-web-markup [self]))

(defrecord BlogPost [title text])

(extend-type BlogPost
  Publishable
  (as-web-markup [self]
    (str (:title self) "-" (:text self))))

(defn publish-to-web [content]
  (println (as-web-markup content)))

(def post (new BlogPost "Titulo" "Text"))
(publish-to-web post)
