# Find a Highest-Scoring Modified Peptide against a Spectrum

This problem involves finding the highest-scoring modified peptide that matches a given mass spectrum. I'll solve this using dynamic programming with the modified peptide scoring approach.

```lisp
(defun find-highest-scoring-modified-peptide (spectrum peptide-mass-table)
  "Find the highest-scoring modified peptide against a given spectrum"
  (let* ((n (length spectrum))
         (scores (make-array (list (1+ n)) :element-type 'fixnum :initial-element 0))
         (peptides (make-array (list (1+ n)) :element-type 'list :initial-element nil))
         (max-score 0)
         (best-peptide nil))
    
    ;; Initialize for empty peptide
    (setf (aref scores 0) 0)
    
    ;; Dynamic programming approach
    (loop for i from 1 to n do
      (let ((current-score 0)
            (current-peptide nil))
        ;; Try all possible amino acids
        (loop for (aa . mass) in peptide-mass-table do
          (let ((prev-index (- i (ceiling mass 1.0))))
            (when (and (>= prev-index 0)
                       (> (aref scores prev-index) current-score))
              (setf current-score (aref scores prev-index))
              (setf current-peptide (append (aref peptides prev-index) (list aa)))))) 
        
        ;; Add the score for the current spectrum position
        (setf (aref scores i) (+ current-score (get-spectrum-score i spectrum)))
        (setf (aref peptides i) current-peptide)
        
        ;; Update maximum
        (when (> (aref scores i) max-score)
          (setf max-score (aref scores i))
          (setf best-peptide (aref peptides i)))))
    
    best-peptide))

(defun get-spectrum-score (index spectrum)
  "Get the score for a specific position in the spectrum"
  (if (and (>= index 1) (< index (length spectrum)))
      (aref spectrum index)
      0))

(defun find-highest-scoring-modified-peptide-optimized (spectrum peptide-mass-table)
  "Optimized version using dynamic programming with backtracking"
  (let* ((n (length spectrum))
         (dp (make-array (list (1+ n)) :element-type 'fixnum :initial-element 0))
         (parent (make-array (list (1+ n)) :element-type 'fixnum :initial-element -1))
         (max-score 0)
         (best-index 0))
    
    ;; Fill DP table
    (loop for i from 1 to n do
      (loop for (aa . mass) in peptide-mass-table do
        (let ((prev-index (- i (ceiling mass 1.0))))
          (when (and (>= prev-index 0)
                     (> (+ (aref dp prev-index) (aref spectrum i)) 
                        (aref dp i)))
            (setf (aref dp i) (+ (aref dp prev-index) (aref spectrum i)))
            (setf (aref parent i) prev-index)))))
    
    ;; Find maximum score
    (loop for i from 1 to n do
      (when (> (aref dp i) max-score)
        (setf max-score (aref dp i))
        (setf best-index i)))
    
    ;; Backtrack to get the peptide
    (let ((peptide '()))
      (loop while (>= best-index 0) do
        (let ((prev-index (aref parent best-index)))
          (when (and (>= prev-index 0) (< prev-index best-index))
            (let ((mass-diff (- best-index prev-index)))
              (loop for (aa . mass) in peptide-mass-table do
                (when (= mass mass-diff)
                  (push aa peptide)
                  (return)))))
          (setf best-index prev-index)))
      (nreverse peptide))))

(defun find-highest-scoring-modified-peptide-complete (spectrum peptide-mass-table)
  "Complete implementation for finding highest scoring modified peptide"
  (let* ((n (length spectrum))
         (dp (make-array (list (1+ n)) :element-type 'fixnum :initial-element 0))
         (backtrack (make-array (list (1+ n)) :element-type 'list :initial-element nil))
         (max-score 0)
         (best-peptide nil))
    
    ;; Initialize
    (setf (aref dp 0) 0)
    
    ;; Fill DP table
    (loop for i from 1 to n do
      (let ((max-score-at-i 0)
            (best-aa nil))
        (loop for (aa . mass) in peptide-mass-table do
          (let ((prev-index (- i (ceiling mass 1.0))))
            (when (and (>= prev-index 0)
                       (> (+ (aref dp prev-index) (aref spectrum i)) max-score-at-i))
              (setf max-score-at-i (+ (aref dp prev-index) (aref spectrum i)))
              (setf best-aa aa))))
        (setf (aref dp i) max-score-at-i)
        (when (and best-aa (or (null best-peptide) (> max-score-at-i max-score)))
          (setf max-score max-score-at-i)
          (setf best-peptide (list best-aa)))))
    
    ;; More complex backtracking for actual peptide sequence
    (let ((peptide '()))
      (loop for i from n downto 1 do
        (loop for (aa . mass) in peptide-mass-table do
          (let ((prev-index (- i (ceiling mass 1.0))))
            (when (and (>= prev-index 0)
                       (= (aref dp i) (+ (aref dp prev-index) (aref spectrum i))))
              (push aa peptide)
              (return)))))
      (nreverse peptide))))

;; Main function to solve the problem
(defun solve-modified-peptide-problem (spectrum peptide-mass-table)
  "Main function to find highest scoring modified peptide"
  (let* ((n (length spectrum))
         (dp (make-array (list (1+ n)) :element-type 'fixnum :initial-element 0))
         (peptide-backtrack (make-array (list (1+ n)) :element-type 'list :initial-element nil))
         (max-score 0)
         (best-peptide nil))
    
    ;; Dynamic programming
    (loop for i from 1 to n do
      (loop for (aa . mass) in peptide-mass-table do
        (let ((prev-index (- i (ceiling mass 1.0))))
          (when (and (>= prev-index 0)
                     (> (+ (aref dp prev-index) (aref spectrum i)) 
                        (aref dp i)))
            (setf (aref dp i) (+ (aref dp prev-index) (aref spectrum i)))
            (setf (aref peptide-backtrack i) (cons aa (aref peptide-backtrack prev-index)))))))
    
    ;; Find best peptide by backtracking
    (let ((best-index (loop for i from 1 to n maximizing (aref dp i) into max-val
                           finally (return (position max-val dp)))))
      (if best-index
          (aref peptide-backtrack best-index)
          '()))))

;; Example usage:
;; (solve-modified-peptide-problem 
;;  '(0 113 128 186 244 299 314 372 427 485 543 601 659 717 775 833 891 949 1007 1065 1123 1181 1239 1297 1355 1413 1471 1529 1587 1645 1703 1761 1819 1877 1935 1993 2051 2109 2167 2225 2283 2341 2399 2457 2515 2573 2631 2689 2747 2805 2863 2921 2979 3037 3095 3153 3211 3269 3327 3385 3443 3501 3559 3617 3675 3733 3791 3849 3907 3965 4023 4081 4139 4197 4255 4313 4371 4429 4487 4545 4603 4661 4719 4777 4835 4893 4951 5009 5067 5125 5183 5241 5299 5357 5415 5473 5531 5589 5647 5705 5763 5821 5879 5937 5995 6053 6111 6169 6227 6285 6343 6401 6459 6517 6575 6633 6691 6749 6807 6865 6923 6981 7039 7097 7155 7213 7271 7329 7387 7445 7503 7561 7619 7677 7735 7793 7851 7909 7967 8025 8083 8141 8199 8257 8315 8373 8431 8489 8547 8605 8663 8721 8779 8837 8895 8953 9011 9069 9127 9185 9243 9301 9359 9417 9475 9533 9591 9649 9707 9765 9823 9881 9939 9997 10055 10113 10171 10229 10287 10345 10403 10461 10519 10577 10635 10693 10751 10809 10867 10925 10983 11041 11099 11157 11215 11273 11331 11389 11447 11505 11563 11621 11679 11737 11795 11853 11911 11969 12027 12085 12143 12201 12259 12317 12375 12433 12491 12549 12607 12665 12723 12781 12839 12897 12955 13013 13071 13129 13187 13245 13303 13361 13419 13477 13535 13593 13651 13709 13767 13825 13883 13941 13999 14057 14115 14173 14231 14289 14347 14405 14463 14521 14579 14637 14695 14753 14811 14869 14927 14985 15043 15101 15159 15217 15275 15333 15391 15449 15507 15565 15623 15681 15739 15797 15855 15913 15971 16029 16087 16145 16203 16261 16319 16377 16435 16493 16551 16609 16667 16725 16783 16841 16899 16957 17015 17073 17131 17189 17247 17305 17363 17421 17479 17537 17595 17653 17711 17769 17827 17885 17943 18001 18059 18117 18175 18233 18291 18349 18407 18465 18523 18581 18639 18697 18755 18813 18871 18929 18987 19045 19103 19161 19219 19277 19335 19393 19451 19509 19567 19625 19683 19741 19799 19857 19915 19973 20031 20089 20147 20205 20263 20321 20379 20437 20495 20553 20611 20669 20727 20785 20843 20901 20959 21017 21075 21133 21191 21249 21307 21365 21423 21481 21539 21597 21655 21713 21771 21829 21887 21945 22003 22061 22119 22177 22235 22293 22351 22409 22467 22525 22583 22641 22699 22757 22815 22873 22931 22989 23047 23105 23163 23221 23279 23337 23395 23453 23511 23569 23627 23685 23743 23801 23859 23917 23975 24033 24091 24149 24207 24265 24323 24381 24439 24497 24555 24613 24671 24729 24787 24845 24903 24961 25019 25077 25135 25193 25251 25309 25367 25425 25483 25541 25599 25657 25715 25773 25831 25889 25947 26005 26063 26121 26179 26237 26295 26353 26411 26469 26527 26585 26643 26701 267