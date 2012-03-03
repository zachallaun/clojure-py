(ns examples.pyside.tutorial.t2)
; Taken from http://qt.gitorious.org/pyside/pyside-examples/blobs/master/examples/tutorial/t2.py
; Translated to clojure-py by Timothy Baldridge 2012


(import '(PySide QtGui))
(import '(PySide QtCore))
(alias 'gui 'PySide.QtGui __name__)
(alias 'core 'PySide.QtCore __name__)


(let [app (gui/QApplication sys/argv)
      quit (gui/QPushButton "Quit")]
      (.resize quit 75 30)
      (.setFont quit (gui/QFont "Times" 18 (.-Bold gui/QFont)))
      (.connect core/QObject quit (core/SIGNAL "clicked()")
                             app (core/SLOT "quit()"))
      (.show quit)
      (sys/exit (.exec_ app)))

