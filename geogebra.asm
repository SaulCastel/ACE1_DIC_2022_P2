.model small

.stack

.data
datos_estudiante db "Saul Castellanos",0ah,
                    "201801178",0ah,
                    "Arquitectura de Computadores y Ensambladores 1",0ah,
                    "Dic. 2022",0ah,"$"

menu db "1. Ingresar ecuacion (Funcion).",0ah,
        "2. Imprimir la funcion almacenada.",0ah,
        "3. Imprimir la derivada de dicha funcion.",0ah,
        "4. Imprimir la integral de la funcion.",0ah,
        "5. Graficar la funcion original, derivada o integral.",0ah,
        "6. Encontrar ceros de la funcion por metodo de Newton.",0ah,
        "7. Encontrar ceros de la funcion por metodo de Steffensen.",0ah,
        "8. Salir de la aplicacion.",0ah,0ah,
        "Escoja una opcion(1-8): $"

menu_1 db "1. Ingresar por termino.",0ah,
          "2. Ingresar ecuacion",0ah,
          "3. Volver.",0ah,0ah,
          "Escoja una opcion(1-3): $"

menu_1_1 db "1. Ingresar ecuacion grado 1.",0ah,
            "2. Ingresar ecuacion grado 2.",0ah,
            "3. Ingresar ecuacion grado 3.",0ah,
            "4. Ingresar ecuacion grado 4.",0ah,
            "5. Ingresar ecuacion grado 5.",0ah,
            "6. Volver.",0ah,0ah,
            "Escoja una opcion(1-6): $"

opcion1 db "esta es la opcion 1$"
opcion2 db "esta es la opcion 2$"
opcion3 db "esta es la opcion 3$"
opcion4 db "esta es la opcion 4$"
opcion5 db "esta es la opcion 5$"
opcion6 db "esta es la opcion 6$"
opcion7 db "esta es la opcion 7$"
option_buffer db 02h,?, 02h dup(0)
wait_msg db "Presione ENTER para continuar$"

.186 
.code
  .startup
  mov AX, @DATA
  mov DS, AX

  call CLS
  push OFFSET datos_estudiante
  call println
  call askConfirmation

  mostrar_menu:
  call CLS
  push OFFSET menu
  call print

  push OFFSET option_buffer
  call input
  cmp option_buffer[2], 31h
  je ingresar_ecuacion
  cmp option_buffer[2], 32h
  je imprimir_funcion
  cmp option_buffer[2], 33h
  je imprimir_derivada
  cmp option_buffer[2], 34h
  je imprimir_integral
  cmp option_buffer[2], 35h
  je graficar_funciones
  cmp option_buffer[2], 36h
  je metodo_newton
  cmp option_buffer[2], 37h
  je metodo_steffensen
  cmp option_buffer[2], 38h
  je end_program
  jmp mostrar_menu

  ingresar_ecuacion:
    call CLS
    push OFFSET opcion1
    call println
    call askConfirmation
    jmp mostrar_menu

  imprimir_funcion:
    call CLS
    push OFFSET opcion2
    call println
    call askConfirmation
    jmp mostrar_menu

  imprimir_derivada:
    call CLS
    push OFFSET opcion3
    call println
    call askConfirmation
    jmp mostrar_menu

  imprimir_integral:
    call CLS
    push OFFSET opcion4
    call println
    call askConfirmation
    jmp mostrar_menu

  graficar_funciones:
    call CLS
    push OFFSET opcion5
    call println
    call askConfirmation
    jmp mostrar_menu

  metodo_newton:
    call CLS
    push OFFSET opcion6
    call println
    call askConfirmation
    jmp mostrar_menu

  metodo_steffensen:
    call CLS
    push OFFSET opcion7
    call println
    call askConfirmation
    jmp mostrar_menu

  end_program:
    call CLS
    .exit 0

;------------------------------------------------------------------------------
println PROC
;Imprime un string a consola seguido de un salto de linea.
;RECIBE:
; [BP+4], direccion en DS de cadena a imprimir
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

;------------------------------------------------------------------------------
print PROC
;Imprime un string a consola.
;RECIBE:
; [BP+4], direccion en DS de cadena a imprimir
;------------------------------------------------------------------------------
  push BP
  mov BP, SP
  push AX
  push DX

  mov AH, 09h     ;imprimir una cadena de texto terminada en '$'
  mov DX, [BP+4]  ;direccion de cadena en memoria en segmento de pila
  int 21h

  pop DX
  pop AX
  pop BP
  ret 2
print ENDP

;------------------------------------------------------------------------------
askConfirmation PROC
;muestra un mensaje en consola y espera a que se presione la tecla Enter.
;------------------------------------------------------------------------------
  push OFFSET wait_msg   ;Imprimir un mensaje en pantalla
  call println

  wait_confirmation:
  mov AH, 00h
  int 16h
  cmp AX, 1c0dh           ;tecla Enter
  je confirmation
  jmp wait_confirmation
  confirmation:
  ret
askConfirmation ENDP

;------------------------------------------------------------------------------
CLS PROC uses AX
;Limpia la pantalla
;------------------------------------------------------------------------------
  mov AX, 03h
  int 10h
  ret
CLS ENDP

;------------------------------------------------------------------------------
input PROC
;Solicita entrada de texto al usuario.
;RECIBE:
; [BP+4], direccion en DS de buffer de entrada
;------------------------------------------------------------------------------
  push BP
  mov BP, SP
  push BX
  push DX
  push SI

  mov DX, [BP+4]
  mov AH, 0ah
  int 21h

  xor BX, BX
  xor SI, SI
  mov BX, DX
  mov SI, [BX][1]
  mov byte ptr[BX][SI]+2, '$'

  pop SI
  pop DX
  pop BX
  pop BP
  ret 2
input ENDP

end
