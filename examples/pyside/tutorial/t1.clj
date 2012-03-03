(ns examples.pyside.tutorial.t1)

(import '(PySide QtGui))
(alias 'qt 'PySide.QtGui __name__)


(let [app (qt/QApplication  sys/argv)
      hello (qt/QPushButton "Hello world!")]
      (.resize hello 100 30)
      (.show hello)
      (sys/exit (.exec_ app)))
      

