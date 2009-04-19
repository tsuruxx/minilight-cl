(defsystem minilight
  :description "A simple global illumination renderer "
  :version "0.0.1" 
  :serial t
  :components
  ((:file "vector-3d")
   (:file "minilight")
   (:file "bounding-box")
   (:file "triangle")
   (:file "scene")
   (:file "spatial-index")
   (:file "camera")
   (:file "surface-point")
   (:file "image")
   (:file "raytracer")))
   