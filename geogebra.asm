.model small

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

menu_1 db "1. Ingresar ecuacion grado 1.",0ah,
          "2. Ingresar ecuacion grado 2.",0ah,
          "3. Ingresar ecuacion grado 3.",0ah,
          "4. Ingresar ecuacion grado 4.",0ah,
          "5. Ingresar ecuacion grado 5.",0ah,
          "6. Volver.",0ah,0ah,
          "Escoja una opcion(1-6): $"

;DATOS PARA OPCION 1
terms db 6 dup('C')
term_sign db 6 dup('S')
term_buffer db 4, ?, 4 dup(?)
grade db 0
regex_msg db "Cada termino debe seguir este formato: (+|-)[0-9]{1,2}$"
term_msg db "Ingresa el coeficiente para X^", ?, ': $'
;DATOS PARA OPCION 2
no_function_msg db "No hay ninguna funcion almacenada$"
literal_part db "X^", ?, '$'
coefficient_str db 3 dup(?)
opcion3 db "esta es la opcion 3$"
opcion4 db "esta es la opcion 4$"
opcion5 db "esta es la opcion 5$"
opcion6 db "esta es la opcion 6$"
opcion7 db "esta es la opcion 7$"
option_buffer db 02h,?, 02h dup(0)
wait_msg db "Presione ENTER para continuar$"

.stack

.286 
.code
main PROC
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
    push offset menu_1
    call println
    push offset option_buffer
    call input
    mov al, option_buffer[2]
    cmp al, '0'
    jz ingresar_ecuacion                ;Opcion = 0, invalido
    cmp al, '6'
    jz mostrar_menu                     ;Opcion = 6, volver a menu anterior
    cmp al, '6'
    ja ingresar_ecuacion                ;Opcion > 6, invalido
    call isDigit
    jnz ingresar_ecuacion               ;Opcion no es un digito, invalido
    mov grade, al
    xor ax, ax
    mov al, option_buffer[2]
    sub al, 30h
    push ax
    call inputFunctionTermByTerm
    call askConfirmation
    jmp mostrar_menu

  imprimir_funcion:                     ;Verificar que haya una funcion ingresada
    call CLS
    cmp grade, 0
    jnz stored_function
    push offset no_function_msg
    call println
    call askConfirmation
    jmp mostrar_menu
    stored_function:
    xor si, si
    xor ax, ax
    xor di, di
    xor cl, cl
    mov si, '5'
    mov al, grade
    sub si, ax
    mov di, lengthof literal_part
    sub di, 2
    mov cl, grade
    print_term:
    mov dl, term_sign[si]
    call printChar                      ;Imprimir signo del termino
    mov dl, '('
    call printChar                      ;Imprimir parentesis izquierdo
    push offset coefficient_str
    xor ax, ax
    mov al, terms[si]
    push ax
    call num2str                        ;Convervir coeficiente en un String
    push offset coefficient_str
    call print                          ;Imprimir coeficiente
    mov dl, ')'
    call printChar                      ;Imprimir parentesis derecho
    mov literal_part[di], cl
    push offset literal_part
    call print                          ;Imprimir parte literal del termino
    dec cl
    inc si
    cmp si, 6
    jnz print_term
    mov dl, 0ah
    call printChar                      ;Imprimir un salto de linea
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
main ENDP

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

  mov AH, 09h                           ;imprimir una cadena de texto terminada en '$'
  mov DX, [BP+4]                        ;direccion de cadena en memoria en segmento de pila
  int 21h

  mov AH, 02h                           ;imprimir caracter a pantalla
  mov DL, 0ah                           ;salto de linea
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

  mov AH, 09h                           ;imprimir una cadena de texto terminada en '$'
  mov DX, [BP+4]                        ;direccion de cadena en memoria en segmento de pila
  int 21h

  pop DX
  pop AX
  pop BP
  ret 2
print ENDP

;------------------------------------------------------------------------------
printChar PROC
;Imprime un caracter en pantalla.
;RECIBE:
; dl, codigo ascii
;------------------------------------------------------------------------------
  mov ah, 02h
  int 21h
  ret
printChar ENDP

;------------------------------------------------------------------------------
askConfirmation PROC
;muestra un mensaje en consola y espera a que se presione la tecla Enter.
;------------------------------------------------------------------------------
  push OFFSET wait_msg                  ;Imprimir un mensaje en pantalla
  call println

  wait_confirmation:
  mov AH, 00h
  int 16h
  cmp AX, 1c0dh                         ;tecla Enter
  je confirmation
  jmp wait_confirmation
  confirmation:
  ret
askConfirmation ENDP

;------------------------------------------------------------------------------
CLS PROC uses AX
;Limpia la pantalla
;------------------------------------------------------------------------------
  mov AX, 0003h
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

  xor si, si
  xor bx, bx
  mov si, dx
  mov bl, [si+1]
  mov byte ptr[si][bx+2], '$'

  pop SI
  pop DX
  pop BX
  pop BP
  ret 2
input ENDP

;------------------------------------------------------------------------------
str2num PROC
;Convierte un string en un numero.
;RECIBE:
; [BP+4], direccion de string en DS
; [BP+6], longitud de la string en DS
;RETORNA:
; AX, numero equivalente a la cadena
;------------------------------------------------------------------------------
  push bp
  mov bp, sp
  sub sp, 2
  push SI
  push BX

  mov SI, [BP+4]                        ;Obtener direccion de String
  mov word ptr[BP-2], 0ah
  xor AX, AX                            ;Limpiar AX
  mov AL, [SI]                          ;Obtener primer caracter
  sub AL, 30h                           ;Restarle 30h al caracter
  dec byte ptr[BP+6]                    ;Disminuir numero de chars a leer

  conv_char:
  inc SI
  xor BX, BX
  mov BL, [SI]                          ;Tomar siguiente caracter
  sub BL, 30h                           ;Restarle 30h al caracter
  mul word ptr[BP-2]                    ;Multiplicarle 0ah al resultado actual
  add AX,BX                             ;Sumarle caracter actual al resultado
  dec byte ptr[BP+6]                    ;Disminuir numero de chars a leer
  cmp byte ptr[BP+6], 0                 ;Ya leimos todos los caracteres?
  jne conv_char                         ;No, repetir hasta leer todos los caracteres

  pop BX
  pop SI
  mov sp, bp
  pop bp
  ret 4
str2num ENDP

;------------------------------------------------------------------------------
num2str PROC
;Convierte un numero en una String.
;RECIBE:
; [BP+4], numero a convertir en String
; [BP+6], direccion de almacenamiento de String en DS
;------------------------------------------------------------------------------
  push bp
  mov bp, sp
  sub sp, 4                             ;espacio para 2 variables locales
  push AX
  push DX
  push SI

  mov word ptr[BP-2], 0                 ;conteoNum = 0
  mov word ptr[BP-4], 0ah               ;divisor = 10
  mov AX, [BP+4]                        ;Obtener el numero a convertir en String

  conv_num:
  xor DX, DX                            ;Limpiar DX
  div word ptr[BP-4]                    ;ax/divisor
  push DX                               ;Enviar el resisudo a la pila
  inc word ptr[BP-2]                    ;conteoNum++
  cmp AX, 0                             ;Ya llegamos al ultimo digito del numero?
  jne conv_num                          ;No, repetir hasta llegar al ultimo digito

  xor SI, SI
  mov SI, [BP+6]                        ;Obtener direccion de almacenamiento para String

  store_chars:
  pop DX                                ;Obtener digito al tope de la pila
  add DX, 30h                           ;Sumarle 30h al digito para convertirlo en caracter
  mov byte ptr[SI], DL                  ;Almacenar caracter en memoria
  dec word ptr[BP-2]                    ;conteoNum = conteoNum-1
  inc SI                                ;Apuntar a siguiente posicion en memoria
  cmp byte ptr[BP-2], 0                 ;conteoNum == 0
  jne store_chars                       ;no, repetir hasta sacar todos los numeros

  mov byte ptr[si], '$'
  pop SI
  pop DX
  pop AX
  mov sp, bp
  pop bp
  ret 4
num2str ENDP

;------------------------------------------------------------------------------
isDigit PROC
;Verifica si el caracter en AL es un digito.
;RECIBE:
; al, caracter a verificar
;ENTREGA:
; zf=1, el caracter es un digito
;------------------------------------------------------------------------------
  cmp al, '0'                           ;Numero menor que 0?
  jb not_a_digit                        ;Si, no es digito
  cmp al, '9'                           ;Numero mayor que 9?
  ja not_a_digit                        ;Si, no es digito
  test ax, 0                            ;Setear zf
  not_a_digit:
  ret
isDigit ENDP

;------------------------------------------------------------------------------
validateCoefficient PROC
;Validar la sintaxis de un termino.
;Cada termino es de la forma (+|-)[0-9]{1-2}
;RECIBE:
; [bp+4], direccion en DS con coeficiente a evaluar
; [bp+6], longitud del coeficiente
;ENTREGA:
; zf=1, el coeficiente es valido
;------------------------------------------------------------------------------
  push bp
  mov bp, sp
  push si

  xor si, si
  stateA:
  mov si, [bp+4]
  dec byte ptr[bp+6]
  cmp byte ptr[si], '+'
  jz stateB
  cmp byte ptr[si], '-'
  jz stateB
  mov al, 1
  test al, 1
  jmp quit_val_coe

  stateB:
  inc si
  mov al, [si]
  dec byte ptr[bp+6]
  call isDigit
  jz stateC
  mov al, 1
  test al, 1
  jmp quit_val_coe

  stateC:
  cmp byte ptr[bp+6], 0
  jz quit_val_coe
  inc si
  mov al, [si]
  dec byte ptr[bp+6]
  call isDigit
  jz stateC
  mov al, 1
  test al, 1

  quit_val_coe:
  pop si
  pop bp
  ret 4
validateCoefficient ENDP

;------------------------------------------------------------------------------
inputFunctionTermByTerm PROC
;Entrada de una funcion grado 'n' ingresando cada termino por separado.
;Donde 1 < n < 5
;RECIBE:
; [BP+4], grado de la funcion
;------------------------------------------------------------------------------
  push bp
  mov bp, sp
  sub sp, 2
  push si
  push di
  push bx
  xor ax, ax
  xor bx, bx
  xor di, di
  xor si, si

  mov ax, 5
  mov bx, [bp+4]                        ;Obtener grado de la funcion
  mov word ptr[bp-2], bx                ;Almacenar localmente grado del polinomio
  add word ptr[bp-2], 1                 ;Sumarle 1 al grado local
  sub ax, bx                            ;5-grado = corrimiento desde inicio de array de coeficientes
  add si, ax                            ;Obtener posicion en array para el coeficiente actual
  xor bx, bx
  mov bx, offset term_msg               ;Mensaje de termino actual
  mov di, lengthof term_msg             ;Obtener longitud de mensaje
  sub di, 4                             ;Apuntar di hacia 4 caracteres antes del final de cadena
  jmp store_terms

  invalid_term:
  push offset regex_msg                 ;Avisar que el termino ingresado es invalido
  call println
  store_terms:
  mov al, byte ptr[bp+4]                ;Obtener exponente del termino actual
  add al, 30h                           ;Convertir exponente en caracter
  mov [bx+di], al                       ;Modificar mensaje del termino actual -> "X^{al}"
  push bx                               ;ptr_string = mensaje del termino actual
  call print                            ;print(word ptr_string)
  push offset term_buffer               ;ptr_input_buffer = input buffer para coeficiente
  call input                            ;input(word ptr_input_buffer)
  mov dl, 0ah
  call printChar
  push word ptr term_buffer[1]          ;size = longitud del coeficiente(incluyendo signo)
  push offset term_buffer[2]            ;ptr_coefficient = direccion de coeficiente en DS
  call validateCoefficient              ;validateCoefficient(word ptr_coefficient, byte size)
  jnz invalid_term                      ;No es coeficiente valido, volver a solicitar
  mov al, term_buffer[2]                ;Obtener signo del coeficiente
  mov byte ptr term_sign[si], al        ;Almacenar signo en memoria
  xor ax, ax
  mov al, term_buffer[1]                ;Obtener longitud de coeficiente
  sub al, 1                             ;Restarle 1 a longitud para descartar signo
  push ax                               ;num_digitos = numero de digitos en input buffer
  push offset term_buffer[3]            ;ptr_string = cadena de texto con el coeficiente del termino
  call str2num                          ;str2num(word ptr_string, byte num_digitos)
  mov terms[si], al                     ;Almacernar coeficiente en array
  inc si                                ;Apuntar a siguiente posicion de almacenamiento de coeficientes
  dec word ptr[bp+4]
  dec word ptr[bp-2]
  cmp byte ptr[bp-2], 0                 ;Ya se obtuvo el ultimo coeficiente?
  jne store_terms                       ;No, repetir hasta obtener el ultimo coeficiente

  pop bx
  pop di
  pop si
  mov sp, bp
  pop bp
  ret 2
inputFunctionTermByTerm ENDP

end main
