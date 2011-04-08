
(module mred mzscheme
  (require (lib "class.ss")
           "../class-iop.ss"
           (prefix mr: (lib "mred.ss" "mred")))
  
  ;  (require-for-syntax "../stx.ss")
  ;  (require (lib "list.ss")
  ;           (lib "match.ss")
  ;           (lib "pretty.ss"))
  ;  (define (names iface)
  ;    (quicksort (interface->method-names iface)
  ;               (lambda (a b)
  ;                 (string<=? (symbol->string a) (symbol->string b)))))
  ;  (define-syntax (go stx)
  ;    (syntax-case stx ()
  ;      [(go (name vname))
  ;       #`(pretty-print 
  ;          `(define-static-interface name ,(sym+ 'mr: 'name)
  ;             ,(names vname)))]
  ;      [(go name)
  ;       (with-syntax ([vname (stx@ #'name (sym+ 'mr: #'name))])
  ;         #'(go (name vname)))]
  ;      [(go name ...)
  ;       #`(begin (go name) ...)]))
  ;  
  ;  (go area<%>
  ;      area-container<%>
  ;      canvas<%>
  ;      clipboard<%>
  ;      control<%>
  ;      labelled-menu-item<%>
  ;      list-control<%>
  ;      menu-item<%>
  ;      menu-item-container<%>
  ;      selectable-menu-item<%>
  ;      subarea<%>
  ;      subwindow<%>
  ;      top-level-window<%>
  ;      window<%>
  ;      
  ;      color-database<%>
  ;      dc<%>
  ;      font-name-directory<%>
  ;      gl-context<%>
  ;      
  ;      add-color<%>
  ;      editor<%>
  ;      editor-data-class-list<%>
  ;      editor-snip-editor-admin<%>
  ;      mult-color<%>
  ;      readable-snip<%>
  ;      snip-class-list<%>
  ;      style<%>
  ;      )
  
  (define-static-interface area<%> mr:area<%>
    (get-graphical-min-size
     get-parent
     get-top-level-window
     min-height
     min-width
     stretchable-height
     stretchable-width))
  
  (define-static-interface area-container<%> mr:area-container<%>
    (add-child
     after-new-child
     begin-container-sequence
     border
     change-children
     container-flow-modified
     container-size
     delete-child
     end-container-sequence
     get-alignment
     get-children
     get-graphical-min-size
     get-parent
     get-top-level-window
     min-height
     min-width
     place-children
     reflow-container
     set-alignment
     spacing
     stretchable-height
     stretchable-width))
  
  (define-static-interface canvas<%> mr:canvas<%>
    (accept-drop-files
     client->screen
     enable
     focus
     get-canvas-background
     get-client-size
     get-cursor
     get-dc
     get-graphical-min-size
     get-handle
     get-height
     get-label
     get-parent
     get-plain-label
     get-size
     get-top-level-window
     get-width
     get-x
     get-y
     has-focus?
     horiz-margin
     is-enabled?
     is-shown?
     min-client-height
     min-client-width
     min-height
     min-width
     on-char
     on-drop-file
     on-event
     on-focus
     on-move
     on-paint
     on-scroll
     on-size
     on-subwindow-char
     on-subwindow-event
     on-superwindow-enable
     on-superwindow-show
     on-tab-in
     popup-menu
     refresh
     screen->client
     set-canvas-background
     set-cursor
     set-label
     set-resize-corner
     show
     stretchable-height
     stretchable-width
     vert-margin
     warp-pointer))
  
  (define-static-interface clipboard<%> mr:clipboard<%>
    (get-clipboard-bitmap
     get-clipboard-data
     get-clipboard-string
     set-clipboard-bitmap
     set-clipboard-client
     set-clipboard-string))
  
  (define-static-interface control<%> mr:control<%>
    (accept-drop-files
     client->screen
     command
     enable
     focus
     get-client-size
     get-cursor
     get-font
     get-graphical-min-size
     get-handle
     get-height
     get-label
     get-parent
     get-plain-label
     get-size
     get-top-level-window
     get-width
     get-x
     get-y
     has-focus?
     horiz-margin
     is-enabled?
     is-shown?
     min-height
     min-width
     on-drop-file
     on-focus
     on-move
     on-size
     on-subwindow-char
     on-subwindow-event
     on-superwindow-enable
     on-superwindow-show
     popup-menu
     refresh
     screen->client
     set-cursor
     set-label
     show
     stretchable-height
     stretchable-width
     vert-margin))
  
  (define-static-interface labelled-menu-item<%> mr:labelled-menu-item<%>
    (delete
     enable
     get-help-string
     get-label
     get-parent
     get-plain-label
     is-deleted?
     is-enabled?
     on-demand
     restore
     set-help-string
     set-label))
  
  (define-static-interface list-control<%> mr:list-control<%>
    (accept-drop-files
     append
     clear
     client->screen
     command
     enable
     find-string
     focus
     get-client-size
     get-cursor
     get-font
     get-graphical-min-size
     get-handle
     get-height
     get-label
     get-number
     get-parent
     get-plain-label
     get-selection
     get-size
     get-string
     get-string-selection
     get-top-level-window
     get-width
     get-x
     get-y
     has-focus?
     horiz-margin
     is-enabled?
     is-shown?
     min-height
     min-width
     on-drop-file
     on-focus
     on-move
     on-size
     on-subwindow-char
     on-subwindow-event
     on-superwindow-enable
     on-superwindow-show
     popup-menu
     refresh
     screen->client
     set-cursor
     set-label
     set-selection
     set-string-selection
     show
     stretchable-height
     stretchable-width
     vert-margin))
  
  (define-static-interface menu-item<%> mr:menu-item<%>
    (delete 
     get-parent
     is-deleted?
     restore))
  
  (define-static-interface menu-item-container<%> mr:menu-item-container<%>
    (get-items
     on-demand))
  
  (define-static-interface selectable-menu-item<%> mr:selectable-menu-item<%>
    (command
     delete
     enable
     get-help-string
     get-label
     get-parent
     get-plain-label
     get-shortcut
     get-x-shortcut-prefix
     is-deleted?
     is-enabled?
     on-demand
     restore
     set-help-string
     set-label
     set-shortcut
     set-x-shortcut-prefix))
  
  (define-static-interface subarea<%> mr:subarea<%>
    (get-graphical-min-size
     get-parent
     get-top-level-window
     horiz-margin
     min-height
     min-width
     stretchable-height
     stretchable-width
     vert-margin))
  
  (define-static-interface subwindow<%> mr:subwindow<%>
    (accept-drop-files
     client->screen
     enable
     focus
     get-client-size
     get-cursor
     get-graphical-min-size
     get-handle
     get-height
     get-label
     get-parent
     get-plain-label
     get-size
     get-top-level-window
     get-width
     get-x
     get-y
     has-focus?
     horiz-margin
     is-enabled?
     is-shown?
     min-height
     min-width
     on-drop-file
     on-focus
     on-move
     on-size
     on-subwindow-char
     on-subwindow-event
     on-superwindow-enable
     on-superwindow-show
     popup-menu
     refresh
     screen->client
     set-cursor
     set-label
     show
     stretchable-height
     stretchable-width
     vert-margin))
  
  (define-static-interface top-level-window<%> mr:top-level-window<%>
    (accept-drop-files
     add-child
     after-new-child
     begin-container-sequence
     border
     can-close?
     can-exit?
     center
     change-children
     client->screen
     container-flow-modified
     container-size
     delete-child
     enable
     end-container-sequence
     focus
     get-alignment
     get-children
     get-client-size
     get-cursor
     get-edit-target-object
     get-edit-target-window
     get-eventspace
     get-focus-object
     get-focus-window
     get-graphical-min-size
     get-handle
     get-height
     get-label
     get-parent
     get-plain-label
     get-size
     get-top-level-window
     get-width
     get-x
     get-y
     has-focus?
     is-enabled?
     is-shown?
     min-height
     min-width
     move
     on-activate
     on-close
     on-drop-file
     on-exit
     on-focus
     on-message
     on-move
     on-size
     on-subwindow-char
     on-subwindow-event
     on-superwindow-enable
     on-superwindow-show
     on-system-menu-char
     on-traverse-char
     place-children
     popup-menu
     reflow-container
     refresh
     resize
     screen->client
     set-alignment
     set-cursor
     set-label
     show
     spacing
     stretchable-height
     stretchable-width))
  
  (define-static-interface window<%> mr:window<%>
    (accept-drop-files
     client->screen
     enable
     focus
     get-client-size
     get-cursor
     get-graphical-min-size
     get-handle
     get-height
     get-label
     get-parent
     get-plain-label
     get-size
     get-top-level-window
     get-width
     get-x
     get-y
     has-focus?
     is-enabled?
     is-shown?
     min-height
     min-width
     on-drop-file
     on-focus
     on-move
     on-size
     on-subwindow-char
     on-subwindow-event
     on-superwindow-enable
     on-superwindow-show
     popup-menu
     refresh
     screen->client
     set-cursor
     set-label
     show
     stretchable-height
     stretchable-width))
  
  (define-static-interface color-database<%> mr:color-database<%>
    (find-color))
  
  (define-static-interface dc<%> mr:dc<%>
    (clear
     draw-arc
     draw-bitmap
     draw-bitmap-section
     draw-ellipse
     draw-line
     draw-lines
     draw-path
     draw-point
     draw-polygon
     draw-rectangle
     draw-rounded-rectangle
     draw-spline
     draw-text
     end-doc
     end-page
     get-background
     get-brush
     get-char-height
     get-char-width
     get-clipping-region
     get-font
     get-gl-context
     get-origin
     get-pen
     get-scale
     get-size
     get-smoothing
     get-text-background
     get-text-extent
     get-text-foreground
     get-text-mode
     glyph-exists?
     ok?
     set-background
     set-brush
     set-clipping-rect
     set-clipping-region
     set-font
     set-origin
     set-pen
     set-scale
     set-smoothing
     set-text-background
     set-text-foreground
     set-text-mode
     start-doc
     start-page
     try-color))
  
  (define-static-interface font-name-directory<%> mr:font-name-directory<%>
    (find-family-default-font-id
     find-or-create-font-id
     get-face-name
     get-family
     get-font-id
     get-post-script-name
     get-screen-name
     set-post-script-name
     set-screen-name))
  
  (define-static-interface gl-context<%> mr:gl-context<%>
    (call-as-current
     ok?
     swap-buffers))
  
  (define-static-interface add-color<%> mr:add-color<%>
    (get
     get-b
     get-g
     get-r
     set
     set-b
     set-g
     set-r))
  
  (define-static-interface editor<%> mr:editor<%>
    (add-canvas
     add-undo
     adjust-cursor
     after-edit-sequence
     after-load-file
     after-save-file
     auto-wrap
     begin-edit-sequence
     begin-write-header-footer-to-file
     blink-caret
     can-do-edit-operation?
     can-load-file?
     can-save-file?
     change-style
     clear
     clear-undos
     copy
     copy-self
     copy-self-to
     cut
     dc-location-to-editor-location
     default-style-name
     do-edit-operation
     editor-location-to-dc-location
     end-edit-sequence
     end-write-header-footer-to-file
     find-first-snip
     find-scroll-line
     get-active-canvas
     get-admin
     get-canvas
     get-canvases
     get-dc
     get-descent
     get-extent
     get-file
     get-filename
     get-flattened-text
     get-focus-snip
     get-inactive-caret-threshold
     get-keymap
     get-load-overwrites-styles
     get-max-height
     get-max-undo-history
     get-max-view-size
     get-max-width
     get-min-height
     get-min-width
     get-paste-text-only
     get-snip-data
     get-snip-location
     get-space
     get-style-list
     get-view-size
     global-to-local
     in-edit-sequence?
     insert
     insert-box
     insert-file
     insert-image
     insert-port
     invalidate-bitmap-cache
     is-locked?
     is-modified?
     kill
     load-file
     local-to-global
     locations-computed?
     lock
     locked-for-flow?
     locked-for-read?
     locked-for-write?
     needs-update
     num-scroll-lines
     on-change
     on-char
     on-default-char
     on-default-event
     on-display-size
     on-display-size-when-ready
     on-edit-sequence
     on-event
     on-focus
     on-load-file
     on-local-char
     on-local-event
     on-new-box
     on-new-image-snip
     on-paint
     on-save-file
     on-snip-modified
     own-caret
     paste
     paste-x-selection
     print
     print-to-dc
     put-file
     read-footer-from-file
     read-from-file
     read-header-from-file
     redo
     refresh
     refresh-delayed?
     release-snip
     remove-canvas
     resized
     save-file
     save-port
     scroll-line-location
     scroll-to
     select-all
     set-active-canvas
     set-admin
     set-caret-owner
     set-cursor
     set-filename
     set-inactive-caret-threshold
     set-keymap
     set-load-overwrites-styles
     set-max-height
     set-max-undo-history
     set-max-width
     set-min-height
     set-min-width
     set-modified
     set-paste-text-only
     set-snip-data
     set-style-list
     style-has-changed
     undo
     write-footers-to-file
     write-headers-to-file
     write-to-file))
  
  (define-static-interface editor-data-class-list<%> mr:editor-data-class-list<%>
    (add
     find
     find-position
     nth
     number))
  
  (define-static-interface editor-snip-editor-admin<%> mr:editor-snip-editor-admin<%>
    (get-dc
     get-max-view
     get-snip
     get-view
     grab-caret
     modified
     needs-update
     popup-menu
     refresh-delayed?
     resized
     scroll-to
     update-cursor))
  
  (define-static-interface mult-color<%> mr:mult-color<%>
    (get
     get-b
     get-g
     get-r
     set
     set-b
     set-g
     set-r))
  
  (define-static-interface readable-snip<%> mr:readable-snip<%> 
    (read-special))
  
  (define-static-interface snip-class-list<%> mr:snip-class-list<%>
    (add
     find
     find-position
     nth
     number))
  
  (define-static-interface style<%> mr:style<%>
    (get-alignment
     get-background
     get-base-style
     get-delta
     get-face
     get-family
     get-font
     get-foreground
     get-name
     get-shift-style
     get-size
     get-size-in-pixels
     get-smoothing
     get-style
     get-text-descent
     get-text-height
     get-text-space
     get-text-width
     get-transparent-text-backing
     get-underlined
     get-weight
     is-join?
     set-base-style
     set-delta
     set-shift-style
     switch-to))
  
  )
