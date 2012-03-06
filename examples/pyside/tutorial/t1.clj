(ns examples.pyside.tutorial.t1
    (:require [PySide.QtGui :only [QApplication QPushButton]]))




(let [app (QApplication  sys/argv)
      hello (QPushButton "Hello world!")]
      (.resize hello 100 30)
      (.show hello)
      (sys/exit (.exec_ app)))
      

