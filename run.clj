(use 'ring.adapter.jetty)
(require '[discwar.ai :as ai])

(run-jetty (ai/app) {:port 9090})
