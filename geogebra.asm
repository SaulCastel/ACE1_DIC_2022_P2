.model small

.stack

.data
hello db "hola mundo!$"

.186 
.code
  .startup
  mov AX, @DATA
  mov DS, AX

  push OFFSET hello
  call println
  .exit 0

;------------------------------------------------------------------------------
println PROC
;Imprime un string a consola seguido de un salto de linea.
;RECIBE:
; [BP+4], direccion de cadena a imprimir
;------------------------------------------------------------------------------
  push BP
  mov BP, SP
  push AX
  push DX

  mov AH, 09h     ;imprimir una cadena de texto terminada en '$'
  mov DX, [BP+4]  ;direccion de cadena en memoria en segmento de pila
  int 21h

  mov AH, 02h      ;imprimir caracter a pantalla
  mov DL, 0ah     ;salto de linea
  int 21h

  pop DX
  pop AX
  pop BP
  ret 2
println ENDP

end
