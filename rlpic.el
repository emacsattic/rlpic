;;; rlpic.el --- Extension to picture mode for roguelike games

;; Copyright (C) 1999 by Tom Breton

;; Author: Tom Breton <Tehom@localhost>
;; Keywords: local
;; Version: 1.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Picture-mode is handy for editing ASCII maps, such as are sometimes
;; used in roguelike games.  Try: M-x picture-mode <RET> C-h m

;; Even if the map takes up only a part of the file, one can
;; narrow-to-region on that region and edit it in picture mode.

;; Don't forget about the ability to insert the same character
;; multiple times by using the prefix argument.
;; rlpic-insert-feature-character supports this too.

;; I've only included the characters for the map of Rampart.  Other
;; character sets could be created by hand, but IMO it makes more
;; sense to wait for a more programmable way of defining maps and to
;; automatically generate characters from that.

;;; Entry points:

;; rlpic-mode (Dummy function, just forwards to picture mode)
;; rlpic-show-character-meaning
;; rlpic-insert-feature-character
;; rlpic-show-feature-character
;; rlpic-fill-rectangle
;; rlpic-show-all-symbols

;;; Change Log

;; 2000-05-01  Tom Breton  <tob@world.std.com>
;; Fixed insertion bug

;; 2000-05-01  Tom Breton  <tob@world.std.com>
;; Made rlpic-mode dummy entry point.

;; 2000-05-01  Tom Breton  <tob@world.std.com>
;; Added new entry point, rlpic-fill-rectangle

;; 2000-05-01  Tom Breton  <tob@world.std.com>
;; Made modeline always reflect the character point is on.

;; 2000-05-01  Tom Breton  <tob@world.std.com>
;; rlpic-show-all-symbols is now an entry point, with a temp buffer.

;;; Code:

(require 'cl)

;;The characters for a city map, from city.c
(defconst rlpic-city-feature-alist
  '(
     ( "in the garden"                          . ?g )
     ( "in the cemetary"                        . ?y)
     ( "in the temple"                          . ?t)
     ( "in the jail"                            . ?J)
     ( "in the vault"                           . ?v)
     ( "in the vault, with treasure"            . ?V)
     ( "a random minor city location"           . ?x)
     ( "a garden maze"                          . ?\?)
     ( "entrance to the College"                . ?C)
     ( "entrance to the Sorcerors Guild"        . ?s)
     ( "entrance to the Mercenaries Guild"      . ?M)
     ( "entrance to the castle"                 . ?c)
     ( "entrance to the Order Of Paladins"      . ?P)
     ( "entrance to the Hospice"                . ?H)
     ( "entrance to the Arena"                  . ?A)
     ( "entrance to the Bank"                   . ?B)
     ( "exit to the countryside"                . ?X)
     ( "a treasure"                             . ?$)
     ( "a guard"                                . ?G)
     ( "the justiciar"                          . ?j)
     ( "a minor undead monster"                 . ?u)
     ( "a major undead monster"                 . ?U)
     ( "an altar to Odin"                       . ?2)
     ( "an altar to Set"                        . ?3)
     ( "an altar to Athena"                     . ?4)
     ( "an altar to Hecate"                     . ?5)
     ( "an altar to the Lords Of Destiny"       . ?6)
     ( "action: drop the portcullis"            . ?T)
     ( "action: raise the portcullis"           . ?R)
     ( "a steel portcullis"                     . ?7)
     ( "a secret door"                          . ?S)
     ( "a siren trap"                           . ?%)
     ( "a trap"                                 . ?^)
     ( "a hedge"                                . ?\")
     ( "water"                                  . ?\~)
     ( "a magic pool"                           . ?\=)
     ( "a weak wall"                            . ?*)
     ( "a wall"                                 . ?\#)
     ( "the floor"                              . ?\.)
     ( "a secret passage"                       . ?,)
     ( "a closed door"                          . ?-)
     ( "a strange-looking statue"               . ?1)

     )

  "" )


;;;;;;;;
;;Internal variables

(defvar rlpic-feature-alist rlpic-city-feature-alist 
  "Map each feature-name to character." )

;;Variables used to keep mode line up to date.
(defvar rlpic-recent-char nil "" )
(defvar rlpic-recent-feature-text nil "" )
(defvar rlpic-recent-direction-sym nil "" )
(defvar rlpic-recent-direction-name "none" "" )

;;;;;;;;;;;;;;;
;;Types

;;Fake `(defstruct (rplc-cell (:type cons)) description char)'

(defalias 'rlpic-cell-char 'cdr)
(defalias 'rlpic-cell-description 'car)


;;;;;;;;
;;Low-level functions: Strings from data

(defun rlpic-cell-meaning-string (cell)
  ""
  (format "%c is %s" (rlpic-cell-char cell) (rlpic-cell-description cell)))

(defun rlpic-cell-reverse-meaning-string (cell)
  ""
  (format
    "%s is represented by %c"
    (rlpic-cell-description cell)
    (rlpic-cell-char cell)))


(defun rlpic-char-meaning (the-char)
  ""

  (if
    (null the-char)    
    ""
    (let
      ((the-feature-cell (rassoc the-char rlpic-feature-alist)))

      (if
	(null the-feature-cell)
	(format
	  "'%c' currently has no meaning."
	  the-char)
	(rlpic-cell-meaning-string the-feature-cell)))))


;;;;;;;;;;;;;
;;Direction information 

;;Borrowed from picture-mode, cleaned up, and encapsulated
(defun rlpic-direction-sym ()
  ""
  (nth 
    ;;-2 to 2 -> 0 to 4
    (+ 2 (% picture-horizontal-step 3))
    (nth
      ;;-1 to 1 -> 0 to 2
      (+ 1 (% picture-vertical-step 2))
      '( 
	 (wnw  nw   up   ne    ene) 
	 (Left left none right Right)
	 (wsw  sw   down se    ese)))))

;;;;
;;Interaction helper function.

(defun rlpic-get-feature-character ()
  "Return an interactively-chosen element of the terrain-list."

  (let
    ((str
       (completing-read "Which terrain? "
	 rlpic-feature-alist nil t)))

    (when str
      (assoc str rlpic-feature-alist))))


;;;;;;;;;;;;;;
;;Post-command display resynchers

(defun rlpic-reflect-terrain-change ()
  ""
  (let
    ((current-char (char-after)))
    (unless
      (= current-char rlpic-recent-char)

      (setq rlpic-recent-char current-char)
      (setq 
	rlpic-recent-feature-text
	(rlpic-char-meaning current-char))
      (force-mode-line-update))))

(defun rlpic-reflect-direction-change ()
  ""
  
  (let
    ((direction-sym (rlpic-direction-sym)))
    
    (unless
      (eq rlpic-recent-direction-sym direction-sym)

      (setq
	rlpic-recent-direction-sym direction-sym)

      (setq
	rlpic-recent-direction-name
	(symbol-name direction-sym))
  
      (force-mode-line-update))))


(defun rlpic-check-change ()
  ""
  
  ;;Not a real entry point, but used interactively in development.
  (interactive)

  ;;We must not let an error thru because this function is used as
  ;;post-command-hook.
  (ignore-errors
    (rlpic-reflect-terrain-change)
    (rlpic-reflect-direction-change)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;Interactive commands.

;;Not so important now that mode line automatically reflects it.
(defun rlpic-show-character-meaning ()
  "Show what the character point is on means."

  (interactive)

  (message (rlpic-char-meaning (char-after))))




(defun rlpic-show-feature-character (cell)
  "Show the character for an interactively selected terrain."

  (interactive (list (rlpic-get-feature-character)))

  (when
    cell
    (message (rlpic-cell-reverse-meaning-string cell))))


(defun rlpic-insert (ch arg)
  ""
  
  (picture-update-desired-column t)
  (picture-insert ch arg))


(defun rlpic-insert-feature-character (arg)
  "Insert the character for an interactively selected terrain."

  (interactive "p")

  (let*
    ((cell (rlpic-get-feature-character)))

    (when
      cell
      (rlpic-insert (rlpic-cell-char cell) arg))))


(defun rlpic-column-of (p)
  ""
  (save-excursion
    (goto-char p)
    (current-column)))

(defun rlpic-fill-rectangle (start end the-char)
  "Fill rectangle given by START and END with THE-CHAR.
Leaves the region surrounding the rectangle."

  (interactive 
    (list
      (region-beginning) 
      (region-end)
      (rlpic-cell-char (rlpic-get-feature-character))))
  
  (let 
    ( (indent-tabs-mode nil)
      (len
	(- (rlpic-column-of end) (rlpic-column-of start))))
    
    (push-mark)
    (string-rectangle start end (make-string len the-char))))



(defun rlpic-show-all-symbols  (&optional feature-alist)
  "Pop up a buffer describing all the symbols."
  (interactive)

  (if
    (null feature-alist)
    (setq feature-alist rlpic-feature-alist))
  
  (with-output-to-temp-buffer "*rlpic current symbols"
    (princ
      (mapconcat
	#'rlpic-cell-meaning-string
	feature-alist
	"\n"))))


(defun rlpic-mode ()
  "Enter rlpic mode. 
This function mostly just forwards to picture-mode"
  (interactive)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook #'rlpic-check-change nil t)

  ;;Do most of the work
  (picture-mode)

  ;;Set up the mode line to respond to internal data.
  (setq
    mode-line-format
    (list 
      "-" 
      'mode-line-mule-info 
      'mode-line-modified 
      'mode-line-frame-identification 
      'mode-line-buffer-identification
      "[%2l,%2c,"'rlpic-recent-direction-name"]  "
      '(30 . rlpic-recent-feature-text)
      " " 'global-mode-string
      "   %[(" 'mode-name 'mode-line-process 'minor-mode-alist "%n" ")%]--"
      '(-3 . "%p")
      "-%-")))




;;Additional possibilities:

;;Make it a real derived mode from picture-mode instead of cutting
;;corners. 

;;Make it auto-insert by filling a rectangle of a predetermined sie
;;with a predetermined character.  Possibly use define-auto-insert:
;;(define-auto-insert condition action &optional after)

;;rlpic-feature-alist could be chosen from multiple lists when the
;;mode is started, or even read from a file.


;;; rlpic.el ends here