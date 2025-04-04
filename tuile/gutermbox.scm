(define-module (tuile gutermbox)
  #:use-module (ice-9 match)
  #:export
  (
   ;; API - C-versions exported directly from libgutermbox

   get-key
   put-str

   readline-with-input
   readline
   message

   init
   shutdown
   width
   height
   clear
   present
   set-cursor
   hide-cursor
   put-ch
   poll-event
   peek-event

   ;; Constants
   TB_KEY_F1
   TB_KEY_F2
   TB_KEY_F3
   TB_KEY_F4
   TB_KEY_F5
   TB_KEY_F6
   TB_KEY_F7
   TB_KEY_F8
   TB_KEY_F9
   TB_KEY_F10
   TB_KEY_F11
   TB_KEY_F12
   TB_KEY_INSERT
   TB_KEY_DELETE
   TB_KEY_HOME
   TB_KEY_END
   TB_KEY_PGUP
   TB_KEY_PGDN
   TB_KEY_ARROW_UP
   TB_KEY_ARROW_DOWN
   TB_KEY_ARROW_LEFT
   TB_KEY_ARROW_RIGHT
   TB_KEY_MOUSE_LEFT
   TB_KEY_MOUSE_RIGHT
   TB_KEY_MOUSE_MIDDLE
   TB_KEY_MOUSE_RELEASE
   TB_KEY_MOUSE_WHEEL_UP
   TB_KEY_MOUSE_WHEEL_DOWN

   TB_KEY_CTRL_TILDE
   TB_KEY_CTRL_2
   TB_KEY_CTRL_A
   TB_KEY_CTRL_B
   TB_KEY_CTRL_C
   TB_KEY_CTRL_D
   TB_KEY_CTRL_E
   TB_KEY_CTRL_F
   TB_KEY_CTRL_G
   TB_KEY_BACKSPACE
   TB_KEY_CTRL_H
   TB_KEY_TAB
   TB_KEY_CTRL_I
   TB_KEY_CTRL_J
   TB_KEY_CTRL_K
   TB_KEY_CTRL_L
   TB_KEY_ENTER
   TB_KEY_CTRL_M
   TB_KEY_CTRL_N
   TB_KEY_CTRL_O
   TB_KEY_CTRL_P
   TB_KEY_CTRL_Q
   TB_KEY_CTRL_R
   TB_KEY_CTRL_S
   TB_KEY_CTRL_T
   TB_KEY_CTRL_U
   TB_KEY_CTRL_V
   TB_KEY_CTRL_W
   TB_KEY_CTRL_X
   TB_KEY_CTRL_Y
   TB_KEY_CTRL_Z
   TB_KEY_ESC
   TB_KEY_CTRL_LSQ_BRACKET
   TB_KEY_CTRL_3
   TB_KEY_CTRL_4
   TB_KEY_CTRL_BACKSLASH
   TB_KEY_CTRL_5
   TB_KEY_CTRL_RSQ_BRACKET
   TB_KEY_CTRL_6
   TB_KEY_CTRL_7
   TB_KEY_CTRL_SLASH
   TB_KEY_CTRL_UNDERSCORE
   TB_KEY_SPACE
   TB_KEY_BACKSPACE2
   TB_KEY_CTRL_8

   TB_MOD_ALT
   TB_MOD_MOTION

   TB_DEFAULT
   TB_BLACK
   TB_RED
   TB_GREEN
   TB_YELLOW
   TB_BLUE
   TB_MAGENTA
   TB_CYAN
   TB_WHITE

   TB_BOLD
   TB_UNDERLINE
   TB_REVERSE

   TB_EVENT_KEY
   TB_EVENT_RESIZE
   TB_EVENT_MOUSE

   TB_EUNSUPPORTED_TERMINAL
   TB_EFAILED_TO_OPEN_TTY
   TB_EPIPE_TRAP_ERROR

   TB_INPUT_CURRENT
   TB_INPUT_ESC
   TB_INPUT_ALT
   TB_INPUT_MOUSE

   TB_OUTPUT_CURRENT
   TB_OUTPUT_NORMAL
   TB_OUTPUT_256
   TB_OUTPUT_216
   TB_OUTPUT_GRAYSCALE

   ))


(use-modules ((srfi srfi-1) #:select (last take take-right drop-right)))


(load-extension "libgutermbox" "init_gutermbox")

;;;   /* Key constants. See also struct tb_event's key fi
;;     *
;;     * These are a safe subset of terminfo keys, which exist on all popular
;;     * terminals. Termbox uses only them to stay truly portable.
;;     */
;;    #define TB_KEY_F1               (0xFFFF-0)
;;    #define TB_KEY_F2               (0xFFFF-1)
;;    #define TB_KEY_F3               (0xFFFF-2)
;;    #define TB_KEY_F4               (0xFFFF-3)
;;    #define TB_KEY_F5               (0xFFFF-4)
;;    #define TB_KEY_F6               (0xFFFF-5)
;;    #define TB_KEY_F7               (0xFFFF-6)
;;    #define TB_KEY_F8               (0xFFFF-7)
;;    #define TB_KEY_F9               (0xFFFF-8)
;;    #define TB_KEY_F10              (0xFFFF-9)
;;    #define TB_KEY_F11              (0xFFFF-10)
;;    #define TB_KEY_F12              (0xFFFF-11)
;;    #define TB_KEY_INSERT           (0xFFFF-12)
;;    #define TB_KEY_DELETE           (0xFFFF-13)
;;    #define TB_KEY_HOME             (0xFFFF-14)
;;    #define TB_KEY_END              (0xFFFF-15)
;;    #define TB_KEY_PGUP             (0xFFFF-16)
;;    #define TB_KEY_PGDN             (0xFFFF-17)
;;    #define TB_KEY_ARROW_UP         (0xFFFF-18)
;;    #define TB_KEY_ARROW_DOWN       (0xFFFF-19)
;;    #define TB_KEY_ARROW_LEFT       (0xFFFF-20)
;;    #define TB_KEY_ARROW_RIGHT      (0xFFFF-21)
;;    #define TB_KEY_MOUSE_LEFT       (0xFFFF-22)
;;    #define TB_KEY_MOUSE_RIGHT      (0xFFFF-23)
;;    #define TB_KEY_MOUSE_MIDDLE     (0xFFFF-24)
;;    #define TB_KEY_MOUSE_RELEASE    (0xFFFF-25)
;;    #define TB_KEY_MOUSE_WHEEL_UP   (0xFFFF-26)
;;    #define TB_KEY_MOUSE_WHEEL_DOWN (0xFFFF-27)

(define TB_KEY_F1               (- #xFFFF 0))
(define TB_KEY_F2               (- #xFFFF 1))
(define TB_KEY_F3               (- #xFFFF 2))
(define TB_KEY_F4               (- #xFFFF 3))
(define TB_KEY_F5               (- #xFFFF 4))
(define TB_KEY_F6               (- #xFFFF 5))
(define TB_KEY_F7               (- #xFFFF 6))
(define TB_KEY_F8               (- #xFFFF 7))
(define TB_KEY_F9               (- #xFFFF 8))
(define TB_KEY_F10              (- #xFFFF 9))
(define TB_KEY_F11              (- #xFFFF 10))
(define TB_KEY_F12              (- #xFFFF 11))
(define TB_KEY_INSERT           (- #xFFFF 12))
(define TB_KEY_DELETE           (- #xFFFF 13))
(define TB_KEY_HOME             (- #xFFFF 14))
(define TB_KEY_END              (- #xFFFF 15))
(define TB_KEY_PGUP             (- #xFFFF 16))
(define TB_KEY_PGDN             (- #xFFFF 17))
(define TB_KEY_ARROW_UP         (- #xFFFF 18))
(define TB_KEY_ARROW_DOWN       (- #xFFFF 19))
(define TB_KEY_ARROW_LEFT       (- #xFFFF 20))
(define TB_KEY_ARROW_RIGHT      (- #xFFFF 21))
(define TB_KEY_MOUSE_LEFT       (- #xFFFF 22))
(define TB_KEY_MOUSE_RIGHT      (- #xFFFF 23))
(define TB_KEY_MOUSE_MIDDLE     (- #xFFFF 24))
(define TB_KEY_MOUSE_RELEASE    (- #xFFFF 25))
(define TB_KEY_MOUSE_WHEEL_UP   (- #xFFFF 26))
(define TB_KEY_MOUSE_WHEEL_DOWN (- #xFFFF 27))

;;    /* These are all ASCII code points below SPACE character and a BACKSPACE key. */
;;    #define TB_KEY_CTRL_TILDE       0x00
;;    #define TB_KEY_CTRL_2           0x00 /* clash with 'CTRL_TILDE' */
;;    #define TB_KEY_CTRL_A           0x01
;;    #define TB_KEY_CTRL_B           0x02
;;    #define TB_KEY_CTRL_C           0x03
;;    #define TB_KEY_CTRL_D           0x04
;;    #define TB_KEY_CTRL_E           0x05
;;    #define TB_KEY_CTRL_F           0x06
;;    #define TB_KEY_CTRL_G           0x07
;;    #define TB_KEY_BACKSPACE        0x08
;;    #define TB_KEY_CTRL_H           0x08 /* clash with 'CTRL_BACKSPACE' */
;;    #define TB_KEY_TAB              0x09
;;    #define TB_KEY_CTRL_I           0x09 /* clash with 'TAB' */
;;    #define TB_KEY_CTRL_J           0x0A
;;    #define TB_KEY_CTRL_K           0x0B
;;    #define TB_KEY_CTRL_L           0x0C
;;    #define TB_KEY_ENTER            0x0D
;;    #define TB_KEY_CTRL_M           0x0D /* clash with 'ENTER' */
;;    #define TB_KEY_CTRL_N           0x0E
;;    #define TB_KEY_CTRL_O           0x0F
;;    #define TB_KEY_CTRL_P           0x10
;;    #define TB_KEY_CTRL_Q           0x11
;;    #define TB_KEY_CTRL_R           0x12
;;    #define TB_KEY_CTRL_S           0x13
;;    #define TB_KEY_CTRL_T           0x14
;;    #define TB_KEY_CTRL_U           0x15
;;    #define TB_KEY_CTRL_V           0x16
;;    #define TB_KEY_CTRL_W           0x17
;;    #define TB_KEY_CTRL_X           0x18
;;    #define TB_KEY_CTRL_Y           0x19
;;    #define TB_KEY_CTRL_Z           0x1A
;;    #define TB_KEY_ESC              0x1B
;;    #define TB_KEY_CTRL_LSQ_BRACKET 0x1B /* clash with 'ESC' */
;;    #define TB_KEY_CTRL_3           0x1B /* clash with 'ESC' */
;;    #define TB_KEY_CTRL_4           0x1C
;;    #define TB_KEY_CTRL_BACKSLASH   0x1C /* clash with 'CTRL_4' */
;;    #define TB_KEY_CTRL_5           0x1D
;;    #define TB_KEY_CTRL_RSQ_BRACKET 0x1D /* clash with 'CTRL_5' */
;;    #define TB_KEY_CTRL_6           0x1E
;;    #define TB_KEY_CTRL_7           0x1F
;;    #define TB_KEY_CTRL_SLASH       0x1F /* clash with 'CTRL_7' */
;;    #define TB_KEY_CTRL_UNDERSCORE  0x1F /* clash with 'CTRL_7' */
;;    #define TB_KEY_SPACE            0x20
;;    #define TB_KEY_BACKSPACE2       0x7F
;;    #define TB_KEY_CTRL_8           0x7F /* clash with 'BACKSPACE2' */

(define TB_KEY_CTRL_TILDE       #x00)
(define TB_KEY_CTRL_2           #x00) ; clash with 'CTRL_TILDE'
(define TB_KEY_CTRL_A           #x01)
(define TB_KEY_CTRL_B           #x02)
(define TB_KEY_CTRL_C           #x03)
(define TB_KEY_CTRL_D           #x04)
(define TB_KEY_CTRL_E           #x05)
(define TB_KEY_CTRL_F           #x06)
(define TB_KEY_CTRL_G           #x07)
(define TB_KEY_BACKSPACE        #x08)
(define TB_KEY_CTRL_H           #x08) ; clash with 'CTRL_BACKSPACE'
(define TB_KEY_TAB              #x09)
(define TB_KEY_CTRL_I           #x09) ; clash with 'TAB'
(define TB_KEY_CTRL_J           #x0A)
(define TB_KEY_CTRL_K           #x0B)
(define TB_KEY_CTRL_L           #x0C)
(define TB_KEY_ENTER            #x0D)
(define TB_KEY_CTRL_M           #x0D) ; clash with 'ENTER'
(define TB_KEY_CTRL_N           #x0E)
(define TB_KEY_CTRL_O           #x0F)
(define TB_KEY_CTRL_P           #x10)
(define TB_KEY_CTRL_Q           #x11)
(define TB_KEY_CTRL_R           #x12)
(define TB_KEY_CTRL_S           #x13)
(define TB_KEY_CTRL_T           #x14)
(define TB_KEY_CTRL_U           #x15)
(define TB_KEY_CTRL_V           #x16)
(define TB_KEY_CTRL_W           #x17)
(define TB_KEY_CTRL_X           #x18)
(define TB_KEY_CTRL_Y           #x19)
(define TB_KEY_CTRL_Z           #x1A)
(define TB_KEY_ESC              #x1B)
(define TB_KEY_CTRL_LSQ_BRACKET #x1B) ; clash with 'ESC'
(define TB_KEY_CTRL_3           #x1B) ; clash with 'ESC'
(define TB_KEY_CTRL_4           #x1C)
(define TB_KEY_CTRL_BACKSLASH   #x1C) ; clash with 'CTRL_4'
(define TB_KEY_CTRL_5           #x1D)
(define TB_KEY_CTRL_RSQ_BRACKET #x1D) ; clash with 'CTRL_5'
(define TB_KEY_CTRL_6           #x1E)
(define TB_KEY_CTRL_7           #x1F)
(define TB_KEY_CTRL_SLASH       #x1F) ; clash with 'CTRL_7'
(define TB_KEY_CTRL_UNDERSCORE  #x1F) ; clash with 'CTRL_7'
(define TB_KEY_SPACE            #x20)
(define TB_KEY_BACKSPACE2       #x7F)
(define TB_KEY_CTRL_8           #x7F) ; clash with 'BACKSPACE2'

;;
;;    /* These are non-existing ones.
;;     *
;;     * #define TB_KEY_CTRL_1 clash with '1'
;;     * #define TB_KEY_CTRL_9 clash with '9'
;;     * #define TB_KEY_CTRL_0 clash with '0'
;;     */
;;
;;    /*
;;     * Alt modifier constant, see tb_event.mod field and tb_select_input_mode function.
;;     * Mouse-motion modifier
;;     */
;;    #define TB_MOD_ALT    0x01
;;    #define TB_MOD_MOTION 0x02

(define TB_MOD_ALT    #x01)
(define TB_MOD_MOTION #x02)

;;
;;    /* Colors (see struct tb_cell's fg and bg fields). */
;;    #define TB_DEFAULT 0x00
;;    #define TB_BLACK   0x01
;;    #define TB_RED     0x02
;;    #define TB_GREEN   0x03
;;    #define TB_YELLOW  0x04
;;    #define TB_BLUE    0x05
;;    #define TB_MAGENTA 0x06
;;    #define TB_CYAN    0x07
;;    #define TB_WHITE   0x08
;;

(define TB_DEFAULT #x00)
(define TB_BLACK   #x01)
(define TB_RED     #x02)
(define TB_GREEN   #x03)
(define TB_YELLOW  #x04)
(define TB_BLUE    #x05)
(define TB_MAGENTA #x06)
(define TB_CYAN    #x07)
(define TB_WHITE   #x08)

;;    /* Attributes, it is possible to use multiple attributes by combining them
;;     * using bitwise OR ('|'). Although, colors cannot be combined. But you can
;;     * combine attributes and a single color. See also struct tb_cell's fg and bg
;;     * fields.
;;     */
;;    #define TB_BOLD      0x0100
;;    #define TB_UNDERLINE 0x0200
;;    #define TB_REVERSE   0x0400

(define TB_BOLD      #x0100)
(define TB_UNDERLINE #x0200)
(define TB_REVERSE   #x0400)

;;
;;    /* A cell, single conceptual entity on the terminal screen. The terminal screen
;;     * is basically a 2d array of cells. It has the following fields:
;;     *  - 'ch' is a unicode character
;;     *  - 'fg' foreground color and attributes
;;     *  - 'bg' background color and attributes
;;     */
;;    struct tb_cell {
;;      uint32_t ch;
;;      uint16_t fg;
;;      uint16_t bg;
;;    };
;;
;;    #define TB_EVENT_KEY    1
;;    #define TB_EVENT_RESIZE 2
;;    #define TB_EVENT_MOUSE  3

(define TB_EVENT_KEY    1)
(define TB_EVENT_RESIZE 2)
(define TB_EVENT_MOUSE  3)

;;
;;    /* An event, single interaction from the user. The 'mod' and 'ch' fields are
;;     * valid if 'type' is TB_EVENT_KEY. The 'w' and 'h' fields are valid if 'type'
;;     * is TB_EVENT_RESIZE. The 'x' and 'y' fields are valid if 'type' is
;;     * TB_EVENT_MOUSE. The 'key' field is valid if 'type' is either TB_EVENT_KEY
;;     * or TB_EVENT_MOUSE. The fields 'key' and 'ch' are mutually exclusive; only
;;     * one of them can be non-zero at a time.
;;     */
;;    struct tb_event {
;;      uint8_t type;
;;      uint8_t mod; /* modifiers to either 'key' or 'ch' below */
;;      uint16_t key; /* one of the TB_KEY_* constants */
;;      uint32_t ch; /* unicode character */
;;      int32_t w;
;;      int32_t h;
;;      int32_t x;
;;      int32_t y;
;;    };
;;
;;    /* Error codes returned by tb_init(). All of them are self-explanatory, except
;;     * the pipe trap error. Termbox uses unix pipes in order to deliver a message
;;     * from a signal handler (SIGWINCH) to the main event reading loop. Honestly in
;;     * most cases you should just check the returned code as < 0.
;;     */
;;    #define TB_EUNSUPPORTED_TERMINAL -1
;;    #define TB_EFAILED_TO_OPEN_TTY   -2
;;    #define TB_EPIPE_TRAP_ERROR      -3

(define TB_EUNSUPPORTED_TERMINAL -1)
(define TB_EFAILED_TO_OPEN_TTY   -2)
(define TB_EPIPE_TRAP_ERROR      -3)

;;
;;    /* Initializes the termbox library. This function should be called before any
;;     * other functions. Function tb_init is same as tb_init_file("/dev/tty").
;;     * After successful initialization, the library must be
;;     * finalized using the tb_shutdown() function.
;;     */
;;    SO_IMPORT int tb_init(void);
;;    SO_IMPORT int tb_init_file(const char* name);
;;    SO_IMPORT int tb_init_fd(int inout);
;;    SO_IMPORT void tb_shutdown(void);
;;
;;    /* Returns the size of the internal back buffer (which is the same as
;;     * terminal's window size in characters). The internal buffer can be resized
;;     * after tb_clear() or tb_present() function calls. Both dimensions have an
;;     * unspecified negative value when called before tb_init() or after
;;     * tb_shutdown().
;;     */
;;    SO_IMPORT int tb_width(void);
;;    SO_IMPORT int tb_height(void);
;;
;;    /* Clears the internal back buffer using TB_DEFAULT color or the
;;     * color/attributes set by tb_set_clear_attributes() function.
;;     */
;;    SO_IMPORT void tb_clear(void);
;;    SO_IMPORT void tb_set_clear_attributes(uint16_t fg, uint16_t bg);
;;
;;    /* Synchronizes the internal back buffer with the terminal. */
;;    SO_IMPORT void tb_present(void);
;;
;;    #define TB_HIDE_CURSOR -1
;;
;;    /* Sets the position of the cursor. Upper-left character is (0, 0). If you pass
;;     * TB_HIDE_CURSOR as both coordinates, then the cursor will be hidden. Cursor
;;     * is hidden by default.
;;     */
;;    SO_IMPORT void tb_set_cursor(int cx, int cy);
;;
;;    /* Changes cell's parameters in the internal back buffer at the specified
;;     * position.
;;     */
;;    SO_IMPORT void tb_put_cell(int x, int y, const struct tb_cell *cell);
;;    SO_IMPORT void tb_change_cell(int x, int y, uint32_t ch, uint16_t fg, uint16_t bg);
;;
;;    /* Copies the buffer from 'cells' at the specified position, assuming the
;;     * buffer is a two-dimensional array of size ('w' x 'h'), represented as a
;;     * one-dimensional buffer containing lines of cells starting from the top.
;;     *
;;     * (DEPRECATED: use tb_cell_buffer() instead and copy memory on your own)
;;     */
;;    SO_IMPORT void tb_blit(int x, int y, int w, int h, const struct tb_cell *cells);
;;
;;    /* Returns a pointer to internal cell back buffer. You can get its dimensions
;;     * using tb_width() and tb_height() functions. The pointer stays valid as long
;;     * as no tb_clear() and tb_present() calls are made. The buffer is
;;     * one-dimensional buffer containing lines of cells starting from the top.
;;     */
;;    SO_IMPORT struct tb_cell *tb_cell_buffer(void);
;;
;;    #define TB_INPUT_CURRENT 0 /* 000 */
;;    #define TB_INPUT_ESC     1 /* 001 */
;;    #define TB_INPUT_ALT     2 /* 010 */
;;    #define TB_INPUT_MOUSE   4 /* 100 */

(define TB_INPUT_CURRENT 0)  ; 000
(define TB_INPUT_ESC     1)  ; 001
(define TB_INPUT_ALT     2)  ; 010
(define TB_INPUT_MOUSE   4)  ; 100

;;
;;    /* Sets the termbox input mode. Termbox has two input modes:
;;     * 1. Esc input mode.
;;     *    When ESC sequence is in the buffer and it doesn't match any known
;;     *    ESC sequence => ESC means TB_KEY_ESC.
;;     * 2. Alt input mode.
;;     *    When ESC sequence is in the buffer and it doesn't match any known
;;     *    sequence => ESC enables TB_MOD_ALT modifier for the next keyboard event.
;;     *
;;     * You can also apply TB_INPUT_MOUSE via bitwise OR operation to either of the
;;     * modes (e.g. TB_INPUT_ESC | TB_INPUT_MOUSE). If none of the main two modes
;;     * were set, but the mouse mode was, TB_INPUT_ESC mode is used. If for some
;;     * reason you've decided to use (TB_INPUT_ESC | TB_INPUT_ALT) combination, it
;;     * will behave as if only TB_INPUT_ESC was selected.
;;     *
;;     * If 'mode' is TB_INPUT_CURRENT, it returns the current input mode.
;;     *
;;     * Default termbox input mode is TB_INPUT_ESC.
;;     */
;;    SO_IMPORT int tb_select_input_mode(int mode);
;;
;;    #define TB_OUTPUT_CURRENT   0
;;    #define TB_OUTPUT_NORMAL    1
;;    #define TB_OUTPUT_256       2
;;    #define TB_OUTPUT_216       3
;;    #define TB_OUTPUT_GRAYSCALE 4

(define TB_OUTPUT_CURRENT   0)
(define TB_OUTPUT_NORMAL    1)
(define TB_OUTPUT_256       2)
(define TB_OUTPUT_216       3)
(define TB_OUTPUT_GRAYSCALE 4)

;;
;;    /* Sets the termbox output mode. Termbox has three output options:
;;     * 1. TB_OUTPUT_NORMAL     => [1..8]
;;     *    This mode provides 8 different colors:
;;     *      black, red, green, yellow, blue, magenta, cyan, white
;;     *    Shortcut: TB_BLACK, TB_RED, ...
;;     *    Attributes: TB_BOLD, TB_UNDERLINE, TB_REVERSE
;;     *
;;     *    Example usage:
;;     *        tb_change_cell(x, y, '@', TB_BLACK | TB_BOLD, TB_RED);
;;     *
;;     * 2. TB_OUTPUT_256        => [0..256]
;;     *    In this mode you can leverage the 256 terminal mode:
;;     *    0x00 - 0x07: the 8 colors as in TB_OUTPUT_NORMAL
;;     *    0x08 - 0x0f: TB_* | TB_BOLD
;;     *    0x10 - 0xe7: 216 different colors
;;     *    0xe8 - 0xff: 24 different shades of grey
;;     *
;;     *    Example usage:
;;     *        tb_change_cell(x, y, '@', 184, 240);
;;     *        tb_change_cell(x, y, '@', 0xb8, 0xf0);
;;     *
;;     * 3. TB_OUTPUT_216        => [0..216]
;;     *    This mode supports the 3rd range of the 256 mode only.
;;     *    But you don't need to provide an offset.
;;     *
;;     * 4. TB_OUTPUT_GRAYSCALE  => [0..23]
;;     *    This mode supports the 4th range of the 256 mode only.
;;     *    But you dont need to provide an offset.
;;     *
;;     * Execute build/src/demo/output to see its impact on your terminal.
;;     *
;;     * If 'mode' is TB_OUTPUT_CURRENT, it returns the current output mode.
;;     *
;;     * Default termbox output mode is TB_OUTPUT_NORMAL.
;;     */
;;    SO_IMPORT int tb_select_output_mode(int mode);
;;
;;    /* Wait for an event up to 'timeout' milliseconds and fill the 'event'
;;     * structure with it, when the event is available. Returns the type of the
;;     * event (one of TB_EVENT_* constants) or -1 if there was an error or 0 in case
;;     * there were no event during 'timeout' period.
;;     */
;;    SO_IMPORT int tb_peek_event(struct tb_event *event, int timeout);
;;
;;    /* Wait for an event forever and fill the 'event' structure with it, when the
;;     * event is available. Returns the type of the event (one of TB_EVENT_*
;;     * constants) or -1 if there was an error.
;;     */
;;    SO_IMPORT int tb_poll_event(struct tb_event *event);
;;
;;    /* Utility utf8 functions. */
;;    #define TB_EOF -1
;;    SO_IMPORT int tb_utf8_char_length(char c);
;;    SO_IMPORT int tb_utf8_char_to_unicode(uint32_t *out, const char *c);
;;    SO_IMPORT int tb_utf8_unicode_to_char(char *out, uint32_t c);



;; ------------------------------------------------------------
;; Additional functions:


;; Read key-press from terminal.
(define (get-key)

  (define (map-key k)
    (integer->char (if (= (assoc-ref k 'ch) 0)
                       (assoc-ref k 'key)
                       (assoc-ref k 'ch))))

  (let ((k (poll-event)))
    (if (eq? (assoc-ref k 'type) TB_EVENT_KEY)
        (map-key k)
        #f)))


(define (put-str str x y)
  (let ((len (string-length str)))
      (let loop ((i 0)
                 (x x))
        (when (< i len)
          (put-ch x y (string-ref str i))
          (loop (1+ i) (1+ x))))))


;; Read prompted input from "x" and "y".
;;
;; Operates like "readline" library. Only affect the editing line
;; ("y") and cleans everything up.
;;
;; Return-key returns the current input.
;;
;; Escape-key aborts and #f is returned.
;;
;; EOF-type value (#f) is returned, when Ctrl-D is given on empty an
;; line.
;;
(define (readline-with-input prompt input x y)

  (define (show ls rs)

    (let ((all (append ls rs)))

      (cond

       ;; prompt: hii huu jii ju_#
       ((>= (length ls) field-size)
        (let ((text (take-right ls field-size)))
          (put-str (list->string (append text (list #\#)))
                      field-x
                      field-y)
          (set-cursor (+ field-x field-size) field-y)))

       (else
        (let ((text (if (< (length all) field-size)
                        (append all (list #\ ))
                        (append (take all field-size) (list #\#)))))
          (put-str (list->string text)
                      field-x
                      field-y)
          (set-cursor (+ field-x (length ls)) field-y)))))

    (present))

  (define (clean-input ls rs)
    (put-str (make-string (+ (length ls) (length rs)) #\ )
                (+ x (string-length prompt))
                y))

  (define (clean-and-return ret)
    (put-str (make-string (+ (if ret (string-length ret) 0)
                                (string-length prompt))
                             #\ )
                x
                y)
    (hide-cursor)
    (present)
    ret)

  (define field-size (- (width) x (string-length prompt) 2))

  (define field-x (+ x (string-length prompt)))

  (define field-y y)


  (put-str prompt x y)

  (let loop ((ls (if input (string->list input) '()))
             (rs '()))

    (show ls rs)

    (let ((key (get-key))
          (all (append ls rs)))

      (if (char? key)

          (cond

           ;; RETURN
           ((char=? key #\return)
            (clean-and-return (list->string (append ls rs))))

           ;; ESCAPE
           ((char=? key #\escape)
            (clean-and-return #f))

           ;; DELETE / CTRL-H
           ((char=? key #\delete)
            (cond
             ((not (null? ls))
              (loop (drop-right ls 1) rs))
             (else
              (loop ls rs))))

           ;; CTRL-D
           ((char=? key #\eot)
            (cond
             ;; Nothing in cli.
             ((and (null? ls)
                   (null? rs))
              (clean-and-return #f))
             ;; Tail exists.
             ((pair? rs)
              (loop ls (cdr rs)))
             ;; Nothing to delete.
             (else
              (loop ls rs))))

           ;; CTRL-K
           ((char=? key #\vtab)
            (cond
             ;; Tail exists.
             ((pair? rs)
              (clean-input ls rs)
              (loop ls '()))
             ;; Nothing to delete.
             (else
              (loop ls rs))))

           ;; CTRL-B
           ((char=? key #\stx)
            (if (null? ls)
                (loop ls rs)
                (loop (drop-right ls 1)
                      (cons (last ls) rs))))

           ;; CTRL-F
           ((char=? key #\ack)
            (if (null? rs)
                (loop ls rs)
                (loop (append ls (list (car rs)))
                      (cdr rs))))

           ;; CTRL-A
           ((char=? key #\soh)
            (loop '() (append ls rs)))

           ;; CTRL-E
           ((char=? key #\enq)
            (loop all '()))

           (else
            (loop (append ls (list key))
                  rs)))
          #f))))


(define (readline prompt x y)
  (readline-with-input prompt #f x y))


;; Display message to the given terminal position.
;;
;; Display time is 1M us (1s) by default. If 4th arguments is given,
;; that is taken as display time.
;;
(define (message msg x y . rest)
  (let ((time (match rest
                ((time) time)
                (else 1000000))))
    (put-str msg x y)
    (present)
    (usleep time)
    (put-str (make-string (string-length msg) #\ ) x y)
    (present)))
