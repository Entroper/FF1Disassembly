

;;
;;  macro for LDAing an immediate value, then always branching
;;


.macro LDABRA v, lbl
    LDA #v
  .if v = 0
    BEQ lbl
  .else
    BNE lbl
  .endif
.endmacro


;;
;;  macro for quick page boundary assertions
;;

 ; for critical page checks (absolutely should cross page boundary)
.macro CRITPAGECHECK lbl
  .assert >* = >lbl, error, "Critical timing loop crosses page boundary"
.endmacro

 ; for noncritical page checks (crossing page boundary *might* be a problem)
.macro PAGECHECK lbl
  .assert >* = >lbl, warning, "Noncritical timing loop crosses page boundary"
.endmacro