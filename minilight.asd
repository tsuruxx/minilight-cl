(defsystem minilight
  :description "A simple global illumination renderer "
  :version "0.0.1" 
  :serial t
  :components
  ((:file "minilight")
   (:file "vector-3d")
   (:file "triangle")
   (:file "scene")
   (:file "spatial-index")
   (:file "camera")
   (:file "surface-point")
   (:file "image")
   (:file "raytracer")))
   