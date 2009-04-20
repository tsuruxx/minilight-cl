(defsystem minilight
  :description "A simple global illumination renderer "
  :version "0.0.1" 
  :serial t
  :components
  ((:file "vector-3d")
   (:file "minilight")
   (:file "bounding-box")
   (:file "ray")
   (:file "scene")
   (:file "image")
   (:file "camera")
   (:file "triangle")
   (:file "spatial-index")
   (:file "surface-point")
   (:file "raytracer")))
   