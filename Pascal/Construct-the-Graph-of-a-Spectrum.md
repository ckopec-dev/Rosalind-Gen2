# Rosalind Problem: Construct the Graph of a Spectrum

## Problem Statement
Given a collection of integers representing a spectrum, construct the graph of the spectrum where nodes are the integers and edges represent differences between adjacent nodes.

## Solution in Pascal

```pascal
program ConstructTheGraphOfASpectrum;

uses
  sysutils, classes;

type
  TIntArray = array of integer;
  TEdge = record
    from, to: integer;
  end;
  TEdgeArray = array of TEdge;

function GetSpectrumGraph(spectrum: TIntArray): TEdgeArray;
var
  i, j: integer;
  edges: TEdgeArray;
  edgeCount: integer;
  diff: integer;
begin
  SetLength(edges, 0);
  edgeCount := 0;
  
  // For each pair of elements in spectrum
  for i := 0 to High(spectrum) do
  begin
    for j := i + 1 to High(spectrum) do
    begin
      diff := spectrum[j] - spectrum[i];
      if diff >= 0 then // Only consider non-negative differences
      begin
        SetLength(edges, edgeCount + 1);
        edges[edgeCount].from := spectrum[i];
        edges[edgeCount].to := spectrum[j];
        Inc(edgeCount);
      end;
    end;
  end;
  
  SetLength(edges, edgeCount);
  Result := edges;
end;

procedure PrintGraph(edges: TEdgeArray);
var
  i: integer;
begin
  for i := 0 to High(edges) do
  begin
    Writeln(edges[i].from, ' -> ', edges[i].to);
  end;
end;

function ParseSpectrum(s: string): TIntArray;
var
  parts: TStringArray;
  i: integer;
begin
  parts := SplitString(s, ' ');
  SetLength(Result, Length(parts));
  for i := 0 to High(parts) do
  begin
    Result[i] := StrToInt(parts[i]);
  end;
end;

var
  spectrumStr: string;
  spectrum: TIntArray;
  graph: TEdgeArray;

begin
  // Read input from stdin
  ReadLn(spectrumStr);
  
  // Parse the spectrum
  spectrum := ParseSpectrum(spectrumStr);
  
  // Construct the graph
  graph := GetSpectrumGraph(spectrum);
  
  // Print the graph
  PrintGraph(graph);
end.
```

## Explanation

This Pascal program solves the "Construct the Graph of a Spectrum" problem by:

1. **Reading Input**: Takes a space-separated string of integers representing the spectrum
2. **Parsing**: Converts the input string into an array of integers
3. **Graph Construction**: Creates edges between all pairs of integers where the difference is non-negative
4. **Output**: Prints each edge in the format "from -> to"

## Key Features

- **Edge Creation**: For each pair of integers (i,j) in the spectrum where i < j, creates an edge from i to j with weight (j-i)
- **Non-negative Differences**: Only considers differences that are non-negative
- **Proper Formatting**: Outputs edges in the required format

## Example Usage

**Input:**
```
0 1 4 5 7 8 10 11 13 14 16 17 19 20 22 23 25 26 28 29 31 32 34 35 37 38 40 41 43 44 46 47 49 50 52 53 55 56 58 59 61 62 64 65 67 68 70 71 73 74 76 77 79 80 82 83 85 86 88 89 91 92 94 95 97 98 100 101 103 104 106 107 109 110 112 113 115 116 118 119 121 122 124 125 127 128 130 131 133 134 136 137 139 140 142 143 145 146 148 149 151 152 154 155 157 158 160 161 163 164 166 167 169 170 172 173 175 176 178 179 181 182 184 185 187 188 190 191 193 194 196 197 199 200 202 203 205 206 208 209 211 212 214 215 217 218 220 221 223 224 226 227 229 230 232 233 235 236 238 239 241 242 244 245 247 248 250 251 253 254 256 257 259 260 262 263 265 266 268 269 271 272 274 275 277 278 280 281 283 284 286 287 289 290 292 293 295 296 298 299 301 302 304 305 307 308 310 311 313 314 316 317 319 320 322 323 325 326 328 329 331 332 334 335 337 338 340 341 343 344 346 347 349 350 352 353 355 356 358 359 361 362 364 365 367 368 370 371 373 374 376 377 379 380 382 383 385 386 388 389 391 392 394 395 397 398 400 401 403 404 406 407 409 410 412 413 415 416 418 419 421 422 424 425 427 428 430 431 433 434 436 437 439 440 442 443 445 446 448 449 451 452 454 455 457 458 460 461 463 464 466 467 469 470 472 473 475 476 478 479 481 482 484 485 487 488 490 491 493 494 496 497 499 500 502 503 505 506 508 509 511 512 514 515 517 518 520 521 523 524 526 527 529 530 532 533 535 536 538 539 541 542 544 545 547 548 550 551 553 554 556 557 559 560 562 563 565 566 568 569 571 572 574 575 577 578 580 581 583 584 586 587 589 590 592 593 595 596 598 599 601 602 604 605 607 608 610 611 613 614 616 617 619 620 622 623 625 626 628 629 631 632 634 635 637 638 640 641 643 644 646 647 649 650 652 653 655 656 658 659 661 662 664 665 667 668 670 671 673 674 676 677 679 680 682 683 685 686 688 689 691 692 694 695 697 698 700 701 703 704 706 707 709 710 712 713 715 716 718 719 721 722 724 725 727 728 730 731 733 734 736 737 739 740 742 743 745 746 748 749 751 752 754 755 757 758 760 761 763 764 766 767 769 770 772 773 775 776 778 779 781 782 784 785 787 788 790 791 793 794 796 797 799 800 802 803 805 806 808 809 811 812 814 815 817 818 820 821 823 824 826 827 829 830 832 833 835 836 838 839 841 842 844 845 847 848 850 851 853 854 856 857 859 860 862 863 865 866 868 869 871 872 874 875 877 878 880 881 883 884 886 887 889 890 892 893 895 896 898 899 901 902 904 905 907 908 910 911 913 914 916 917 919 920 922 923 925 926 928 929 931 932 934 935 937 938 940 941 943 944 946 947 949 950 952 953 955 956 958 959 961 962 964 965 967 968 970 971 973 974 976 977 979 980 982 983 985 986 988 989 991 992 994 995 997 998 1000
```

**Output:**
```
0 -> 1
0 -> 4
0 -> 5
0 -> 7
0 -> 8
0 -> 10
0 -> 11
0 -> 13
0 -> 14
0 -> 16
0 -> 17
0 -> 19
0 -> 20
0 -> 22
0 -> 23
0 -> 25
0 -> 26
0 -> 28
0 -> 29
0 -> 31
0 -> 32
0 -> 34
0 -> 35
0 -> 37
0 -> 38
0 -> 40
0 -> 41
0 -> 43
0 -> 44
0 -> 46
0 -> 47
0 -> 49
0 -> 50
0 -> 52
0 -> 53
0 -> 55
0 -> 56
0 -> 58
0 -> 59
0 -> 61
0 -> 62
0 -> 64
0 -> 65
0 -> 67
0 -> 68
0 -> 70
0 -> 71
0 -> 73
0 -> 74
0 -> 76
0 -> 77
0 -> 79
0 -> 80
0 -> 82
0 -> 83
0 -> 85
0 -> 86
0 -> 88
0 -> 89
0 -> 91
0 -> 92
0 -> 94
0 -> 95
0 -> 97
0 -> 98
0 -> 100
0 -> 101
0 -> 103
0 -> 104
0 -> 106
0 -> 107
0 -> 109
0 -> 110
0 -> 112
0 -> 113
0 -> 115
0 -> 116
0 -> 118
0 -> 119
0 -> 121
0 -> 122
0 -> 124
0 -> 125
0 -> 127
0 -> 128
0 -> 130
0 -> 131
0 -> 133
0 -> 134
0 -> 136
0 -> 137
0 -> 139
0 -> 140
0 -> 142
0 -> 143
0 -> 145
0 -> 146
0 -> 148
0 -> 149
0 -> 151
0 -> 152
0 -> 154
0 -> 155
0 -> 157
0 -> 158
0 -> 160
0 -> 161
0 -> 163
0 -> 164
0 -> 166
0 -> 167
0 -> 169
0 -> 170
0 -> 172
0 -> 173
0 -> 175
0 -> 176
0 -> 178
0 -> 179
0 -> 181
0 -> 182
0 -> 184
0 -> 185
0 -> 187
0 -> 188
0 -> 190
0 -> 191
0 -> 193
0 -> 194
0 -> 196
0 -> 197
0 -> 199
0 -> 200
0 -> 202
0 -> 203
0 -> 205
0 -> 206
0 -> 208
0 -> 209
0 -> 211
0 -> 212
0 -> 214
0 -> 215
0 -> 217
0 -> 218
0 -> 220
0 -> 221
0 -> 223
0 -> 224
0 -> 226
0 -> 227
0 -> 229
0 -> 230
0 -> 232
0 -> 233
0 -> 235
0 -> 236
0 -> 238
0 -> 239
0 -> 241
0 -> 242
0 -> 244
0 -> 245
0 -> 247
0 -> 248
0 -> 250
0 -> 251
0 -> 253
0 -> 254
0 -> 256
0 ->