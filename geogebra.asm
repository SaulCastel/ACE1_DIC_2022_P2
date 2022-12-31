.286 
.model small

VIDEO_SEGMENT equ 0a000h
VIDEO_PALLETE_PORT equ 03c8h            ;Puerto para seleccionar un indice de color
COLOR_SELECTION_PORT equ 03c9h          ;Puerto para establecer el color del indice
COLOR_INDEX equ 1                       ;Indice de color a establecer
spacing = 1
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

menu_4 db "1. Graficar funcion.",0ah,
          "2. Graficar derivada.",0ah,
          "3. Graficar integral.",0ah,
          "4. Volver.",0ah,0ah,
          "Escoja una opcion(1-4): $"

;DATOS PARA POLINOMIOS
;Coeficientes de las funciones. La 'd' indica el final del arreglo.
fun_coefficients db 6 dup('F'), 'd'     ;Coeficientes de la funcion original
der_coefficients db 5 dup('D'), 'd'     ;Coeficientes de la derividada
int_coefficients db 6 dup('I'), 'd'     ;Coeficientes de la Integral
term_sign db 6 dup('S')                 ;Signos de cada termino
grade dw 0                              ;Grado de la funcion original

;DATOS PARA INGRESAR FUNCIONES
numb_buffer db 4, ?, 4 dup(?)
regex_msg db "Cada termino debe seguir este formato: (+|-)[0-9]{1,2}$"
term_msg db "Ingresa el coeficiente para X^", ?, ': $'

;DATOS PARA IMPRIMIR FUNCIONES
no_function_msg db "No hay ninguna funcion almacenada$"
literal_part db "X^", ?, '$'
coefficient_str db 3 dup(?)
function_msg db "La funcion almacenada:$"
derivative_msg db "La derivada de la funcion:$"
integral_msg db "La integral de la funcion:$"

opcion6 db "esta es la opcion 6$"
opcion7 db "esta es la opcion 7$"
option_buffer db 02h,?, 02h dup(0)
wait_msg db "Presione ENTER para continuar$"

;VARIABLES MODO VIDEO
savedMode db ?
xReal dw ?                              ;Coordenada X de la pantalla
yReal dw ?                              ;Coordenada Y de la pantalla
debug db 0

aprox_method dw 0400h                   ;Aproximacion hacia abajo

.stack

.code
main PROC
  mov AX, @DATA
  mov DS, AX
  fldcw aprox_method                    ;Cambiar el metodo de aproximacion del fpu

  push plotFunction
  pop ax

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
    xor ax, ax
    mov al, option_buffer[2]
    cmp al, '0'
    jz ingresar_ecuacion                ;Opcion = 0, invalido
    cmp al, '6'
    jz mostrar_menu                     ;Opcion = 6, volver a menu anterior
    ja ingresar_ecuacion                ;Opcion > 6, invalido
    call isDigit
    jnz ingresar_ecuacion               ;Opcion no es un digito, invalido
    sub al, 30h
    mov grade, ax                       ;Almacenar grado en memoria
    push ax
    call inputFunctionTermByTerm
    call askConfirmation
    jmp mostrar_menu

  imprimir_funcion:                     ;Verificar que haya una funcion ingresada
    call CLS
    cmp grade, 0
    jnz start_function
    push offset no_function_msg
    call println
    call askConfirmation
    jmp mostrar_menu

    start_function:
    push offset function_msg
    call println

    push grade                          ;exponente: exponente inicial
    push grade                          ;grado: grado de la funcion
    push offset fun_coefficients        ;ptr_coeff: puntero al arreglo de coeficientes
    call printFunction                  ;printFunction(word ptr_coeff, word grado, word exponente)

    call askConfirmation
    jmp mostrar_menu

  imprimir_derivada:
    call CLS
    cmp grade, 0
    jnz start_derivative
    push offset no_function_msg
    call println
    call askConfirmation
    jmp mostrar_menu

    start_derivative:
    push offset function_msg
    call println
    push grade                          ;exponente: exponente inicial
    push grade                          ;grado: grado de la funcion
    push offset fun_coefficients        ;ptr_coeff: puntero al arreglo de coeficientes
    call printFunction                  ;printFunction(word ptr_coeff, word grado, word exponente)

    push offset derivative_msg
    call println
    call calcDerivative
    mov ax, grade
    dec ax
    push ax                             ;exponente: exponente inicial
    push grade                          ;grado: grado de la funcion
    push offset der_coefficients        ;ptr_coeff: puntero al arreglo de coeficientes
    call printFunction                  ;printFunction(word ptr_coeff, word grado, word exponente)

    call askConfirmation
    jmp mostrar_menu

  imprimir_integral:
    call CLS
    cmp grade, 0
    jnz start_integral
    push offset no_function_msg
    call println
    call askConfirmation
    jmp mostrar_menu

    start_integral:
    push offset function_msg
    call println
    push grade                          ;exponente: exponente inicial
    push grade                          ;grado: grado de la funcion
    push offset fun_coefficients        ;ptr_coeff: puntero al arreglo de coeficientes
    call printFunction                  ;printFunction(word ptr_coeff, word grado, word exponente)

    push offset integral_msg
    call println
    call calcIntegral
    mov ax, grade
    inc ax
    push ax                             ;exponente: exponente inicial
    push grade                          ;grado: grado de la funcion
    push offset int_coefficients        ;ptr_coeff: puntero al arreglo de coeficientes
    call printFunction                  ;printFunction(word ptr_coeff, word grado, word exponente)

    call askConfirmation
    jmp mostrar_menu

  graficar_funciones:
    call CLS
    push OFFSET menu_4
    call println

    push offset option_buffer
    call input
    xor ax, ax
    mov al, option_buffer[2]
    call isDigit
    jnz graficar_funciones              ;Opcion no es un digito, invalido
    cmp al, '4'
    jz mostrar_menu                     ;Opcion = 4, volver a menu anterior
    cmp al, '1'
    jb graficar_funciones               ;Opcion < 1, no valido
    cmp al, '3'
    ja graficar_funciones               ;Opcion > 3. no valido

    call videoMode13                    ;Cambiar a modo de video 13h
    call drawPlane                      ;Graficar plano
    push 2
    push -2
    push grade
    push grade
    push offset fun_coefficients
    call plotFunction
    ;Evaluar funcion en rango
    ;Plotear puntos

    wait_for_esc:                       ;Presionar esc para salir de la grafica
    mov ah, 00h
    int 16h
    cmp ax, 011bh
    jnz wait_for_esc

    call restoreVideoMode               ;Restaurar el modo de video anteior
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
;Almacena en memoria la representacion ASCII del valor absoluto de un numero.
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
  test al, al                           ;Es el numero negativo?
  jns conv_num                          ;No, convertir el numero en String
  neg al                                ;Si, convertir numero en positivo

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
  jmp store_fun_coefficients

  invalid_term:
  push offset regex_msg                 ;Avisar que el termino ingresado es invalido
  call println
  store_fun_coefficients:
  mov al, byte ptr[bp+4]                ;Obtener exponente del termino actual
  add al, 30h                           ;Convertir exponente en caracter
  mov [bx+di], al                       ;Modificar mensaje del termino actual -> "X^{al}"
  push bx                               ;ptr_string = mensaje del termino actual
  call print                            ;print(word ptr_string)
  push offset numb_buffer               ;ptr_input_buffer = input buffer para coeficiente
  call input                            ;input(word ptr_input_buffer)
  mov dl, 0ah
  call printChar                        ;Insertar un salto de linea

;# Verificar el coeficiente ingresado
  push word ptr numb_buffer[1]          ;size = longitud del coeficiente(incluyendo signo)
  push offset numb_buffer[2]            ;ptr_coefficient = direccion de coeficiente en DS
  call validateCoefficient              ;validateCoefficient(word ptr_coefficient, byte size)
  jnz invalid_term                      ;No es coeficiente valido, volver a solicitar

;# Almacenar coeficiente en memoria
  xor ax, ax
  mov al, numb_buffer[1]                ;Obtener longitud de coeficiente
  sub al, 1                             ;Restarle 1 a longitud para descartar signo al convertir
  push ax                               ;num_digitos = numero de digitos en input buffer
  push offset numb_buffer[3]            ;ptr_string = cadena de texto con el coeficiente del termino
  call str2num                          ;str2num(word ptr_string, byte num_digitos)
  mov fun_coefficients[si], al          ;Almacernar coeficiente en array
  mov al, numb_buffer[2]                ;Obtener signo del coeficiente
  mov byte ptr term_sign[si], al        ;Almacenar signo en memoria
  cmp al, '-'                           ;Coeficiente negativo?
  jnz next_term                         ;No, continuar con siguiente termino
  neg byte ptr fun_coefficients[si]     ;Si, negar coeficiente almacenado

  next_term:
  inc si                                ;Apuntar a siguiente posicion de almacenamiento de coeficientes
  dec word ptr[bp+4]
  dec word ptr[bp-2]
  cmp byte ptr[bp-2], 0                 ;Ya se obtuvo el ultimo coeficiente?
  jne store_fun_coefficients            ;No, repetir hasta obtener el ultimo coeficiente

  pop bx
  pop di
  pop si
  mov sp, bp
  pop bp
  ret 2
inputFunctionTermByTerm ENDP

;------------------------------------------------------------------------------
printFunction PROC
;Imprimir una funcion como una cadena formateada.
; RECIBE:
; [bp+4], direccion del arreglo con coeficientes de la funcion
; [bp+6], grado de la funcion
; [bp+8], exponente inicial. Los exponentes van en orden decreciente desde
;         este grado
;------------------------------------------------------------------------------
  push bp
  mov bp, sp
  pusha

  xor bx, bx
  mov bl, 5
  mov al, [bp+6]                        ;Obtener grado
  sub bl, al                            ;Corrimiento = 5 - grado
  mov di, offset term_sign              ;Apuntar DI al arreglo de signos
  add di, bx                            ;Posicion en arreglo = indice + corrimiento
  mov si, [bp+4]                        ;Apuntar SI al arreglo de coeficientes
  add si, bx                            ;Posicion en arreglo = indice + corrimiento
  mov cl, [bp+8]                        ;Cargar en CL el exponente inicial

  print_function_term:
  mov dl, [di]
  call printChar                        ;Imprimir signo del termino
  inc di
  mov dl, '('
  call printChar                        ;Imprimir parentesis izquierdo

;# Convertir coeficiente en un string
  push offset coefficient_str           ;ptr_storage: puntero a ds donde almacenar string de coefiente
  xor ax, ax
  mov al, [si]
  push ax                               ;number: numero a convertir en string
  call num2str                          ;num2str(byte number, word ptr_storage)
  inc si

  push offset coefficient_str
  call print                            ;Imprimir coeficiente
  mov dl, ')'
  call printChar                        ;Imprimir parentesis derecho

;# Imprimir parte literal del termino
  mov literal_part[2], cl               ;Indice 2 de literal_part es el byte para el exponente
  add literal_part[2], 30h              ;Convertir exponente en caracter
  push offset literal_part
  call print                            ;Imprimir parte literal del termino
  dec cl

  cmp byte ptr[si], 'd'                 ;Ya llegamos al final de arreglo?
  jnz print_function_term               ;No, repetir impresion de termino
                                        ;Si, terminar procedimiento

  mov dl, 0ah
  call printChar                        ;Imprimir un salto de linea

  popa
  pop bp
  ret 6
printFunction ENDP

calcDerivative PROC uses si cx
  mov si, 5
  mov cx, grade
  sub si, cx
  calc_der_coeff:
  mov al, fun_coefficients[si]          ;Obtener coeficiente actual
  imul cl                               ;Multiplar por el exponente actual
  dec cl                                ;Restarle 1 al exponente
  mov byte ptr der_coefficients[si], al ;Almacenar coeficiente de derivada en memoria
  inc si
  cmp si, 5                             ;Obviamos X^0 porque es constante
  jnz calc_der_coeff
  ret
calcDerivative ENDP

calcIntegral PROC uses si cx
  mov si, 5
  mov cx, grade
  sub si, cx
  inc cx                                ;Sumar 1 al exponente
  calc_int_coeff:
  mov al, fun_coefficients[si]          ;Obtener coeficiente actual
  cbw                                   ;Copiar el msb de AL hacia AH
  idiv cl                               ;Dividimos por el exponente
  mov byte ptr int_coefficients[si], al ;Almacenar cociente en arreglo
  dec cx
  ;TODO: almacenar parte decimal
  inc si
  cmp si, 6
  jnz calc_int_coeff
  ret
calcIntegral ENDP

;------------------------------------------------------------------------------
videoMode13 proc
;Cambiar modo de video a 13h. El registro ES apunta hacia el inicio de la VRAM.
;RECIBE:
; ds:debug =  1, no cambiar el modo de video, pero apuntar ES a VRAM.
;             Esto sirve para depurar el programa en algun depurador
;             como AFD o Insight.
;------------------------------------------------------------------------------
  cmp debug, 1
  jz vram

  mov ah, 0fh                           ;Guardar el modo de video actual
  int 10h
  mov savedMode, al

  mov ax, 0013h                         ;Modo de video 13h (320x200 pixeles, 256 colores)
  int 10h

  vram:
  push VIDEO_SEGMENT
  pop es
  ret
videoMode13 endp

;------------------------------------------------------------------------------
restoreVideoMode proc
;Regresar al modo de video anterior.
;------------------------------------------------------------------------------
  mov ah, 0
  mov al, savedMode
  int 10h
  ret
restoreVideoMode endp

;------------------------------------------------------------------------------
screenCoord PROC
;Realiza el calculo de la posicion en pantalla para las coordenadas (columna,fila).
;RECIBE:
; [bp+4], columna
; [bp+6], fila
;RETORNA:
; AX, posicion en memoria de video
;------------------------------------------------------------------------------
  push bp
  mov bp, sp

  mov ax, 320
  mul word ptr [bp+6]
  add ax, [bp+4]                        ;Posicion = fila * 320 + columna

  pop bp
  ret 4
screenCoord ENDP

;------------------------------------------------------------------------------
setIndexColor MACRO index,red,green,blue
;Cambia el color de un indice de la paleta de colores.
;------------------------------------------------------------------------------
  push dx

  mov dx, VIDEO_PALLETE_PORT            ;Cargar en dx el puerto de seleccion de indice
  mov al, index
  out dx, al                            ;Seleccionar indice

  mov dx, COLOR_SELECTION_PORT          ;Cargar en dx el puerto de la paleta de colores
  mov al, red                           ;Intensidad Rojo
  out dx, al                            ;Enviar a puerto
  mov al, green                         ;Intensidad Verde
  out dx, al                            ;Enviar a puerto
  mov al, blue                          ;Intesidad Azul
  out dx, al                            ;Enviar a puerto

  pop dx
ENDM

;------------------------------------------------------------------------------
drawPlane PROC
;Dibujar el plano cartesiano en pantalla.
;------------------------------------------------------------------------------
  setIndexColor COLOR_INDEX,63,63,63    ;Cambiar indice 1 paleta de colores a blanco

  push 100                              ;Fila
  push 0                                ;Columna
  call screenCoord                      ;screenCoord(word Fila, word Columna)
  mov di, ax
  mov cx, 320
  draw_x:
  mov byte ptr es:[di], COLOR_INDEX
  inc di
  loop draw_x

  push 0
  push 160
  call screenCoord
  mov di, ax
  mov cx, 200
  draw_y:
  mov byte ptr es:[di], COLOR_INDEX
  add di, 320
  loop draw_y

  ret
drawPlane ENDP

;------------------------------------------------------------------------------
plotFunction PROC
;Plotea en pantalla una funcion evaluada sobre un rango.
;RECIBE:
; [bp+4], Direccion en memoria de array con coeficientes
; [bp+6], Grado del polinomio
; [bp+8], exponente inicial
; [bp+10], limite inferior de rango
; [bp+12], limite superior de rango
;------------------------------------------------------------------------------
  push bp
  mov bp, sp
  sub sp, 6
  push si
  push cx

  ;--------VARIABLES LOCALES-----------
  result  equ word ptr [bp-2]           ;Resultado de avaluar polinomio en un valor de X
  xVal    equ word ptr [bp-4]           ;Valor que toma X para evaluar el polinomio
  yVal    equ word ptr [bp-6]           ;Valor que toma Y en el plano cartesiano al plotear puntos
  ;------------------------------------
  mov cx, [bp+10]                       ;Cargar limite inferior
  mov xVal, cx                          ;X inicial es el limite inferior

  range_loop:
  mov result, 0                         ;Limpiar resultado
  mov si, [bp+4]                        ;Indexar array de coeficientes con SI
  add si, 5                             ;Sumarle 5 a la direccion
  sub si, [bp+6]                        ;Corrimiento = indice array + 5 - grado
  mov cx, [bp+8]                        ;Cargar exponente inicial

    term_loop:
    mov ax, xVal                        ;Cargar valor de la X
    dec cx                              ;Disminuir exponente
    cmp cx, 0                           ;Exponente 0?
    jz end_power                        ;Si, Seguir con evaluacion del termino
    imul xVal                           ;No, multiplicar xVal por si mismo
    jmp term_loop
    end_power:
    imul byte ptr [si]                  ;Muliplicar por coeficiente
    add result, ax                      ;Sumar termino al resultado
    inc si                              ;Apuntar a siguiente coeficiente
    cmp byte ptr[si], 'd'               ;Ya llegamos al final del arreglo de coeficientes?
    jnz term_loop                       ;No, repetir hasta llegar al final

  push result                           ;Si, empujar resultado a la pila
  inc xVal                              ;xVal = siguiente valor en el rango
  mov cx, [bp+12]                       ;Cargar limite superior
  cmp xVal, cx                          ;Ya es X mayor que rango superior?
  jna range_loop                        ;No, repetir hasta evaluar el rango entero

  setIndexColor COLOR_INDEX, 63,0,0     ;Color de indice 1 de paleta de colores a Rojo
  loop_plot:
  mov xVal, cx                          ;Cargar X cartesiana de mayor a menor
  pop yVal                              ;Cargar Y cartesiana con ultimo elemento de pila
  mov ax, 160
  add ax, xVal
  mov xReal, ax                         ;Convertir X cartesiana en X de pantalla
  mov ax, 100
  sub ax, yVal
  mov yReal, ax                         ;Convertir Y cartesiana en Y de pantalla
  push yReal                            ;fila
  push xReal                            ;columna
  call screenCoord                      ;screenCoord(word columna, word fila)
  mov di, ax
  mov byte ptr es:[di], COLOR_INDEX     ;Enviar color a coordenada en memoria de video
  dec cx                                ;Decrementamos x
  mov ax, [bp+10]                       ;Cargamos limite inferior
  cmp cx, ax                            ;Ya es X menor que limite inferior?
  jnb loop_plot                         ;No, continuar con ploteo
                                        ;Si, terminar ploteo

  pop cx
  pop si
  mov sp, bp
  pop bp
  ret 10
plotFunction ENDP

end main
