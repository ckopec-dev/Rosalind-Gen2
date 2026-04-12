# Rosalind Problem: Implement_FarthestFirstTraversal in COBOL

## Problem Understanding

The Farthest First Traversal algorithm is a greedy clustering algorithm that:
1. Starts with one center (usually the first point)
2. Repeatedly adds the point farthest from existing centers
3. Continues until k centers are selected

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FARTEST_FIRST_TRAVERSAL.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC A(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC A(100).

       WORKING-STORAGE SECTION.
       01 N PIC 9(4) VALUE 0.
       01 K PIC 9(4) VALUE 0.
       01 I PIC 9(4) VALUE 0.
       01 J PIC 9(4) VALUE 0.
       01 L PIC 9(4) VALUE 0.
       01 M PIC 9(4) VALUE 0.
       01 TEMP PIC 9(4) VALUE 0.
       01 DISTANCE PIC 9(6)V99 VALUE 0.
       01 MIN-DISTANCE PIC 9(6)V99 VALUE 999999.
       01 MAX-DISTANCE PIC 9(6)V99 VALUE 0.
       01 MAX-DISTANCE-INDEX PIC 9(4) VALUE 0.
       01 DISTANCE-ARRAY.
           05 DIST-ITEM OCCURS 100 TIMES PIC 9(6)V99.
       01 POINT-ARRAY.
           05 POINT-ITEM OCCURS 100 TIMES.
               10 X-COORD PIC 9(6)V99.
               10 Y-COORD PIC 9(6)V99.
       01 CENTER-ARRAY.
           05 CENTER-ITEM OCCURS 100 TIMES.
               10 CENTER-X PIC 9(6)V99.
               10 CENTER-Y PIC 9(6)V99.
       01 VISITED-ARRAY.
           05 VISITED OCCURS 100 TIMES PIC 9 VALUE 0.
       01 EOF-FLAG PIC 9 VALUE 0.
       01 LINE-BUFFER PIC A(100).
       01 WORD-1 PIC A(20).
       01 WORD-2 PIC A(20).
       01 WORD-3 PIC A(20).
       01 WORD-4 PIC A(20).
       01 WORD-5 PIC A(20).
       01 WORD-6 PIC A(20).
       01 WORD-7 PIC A(20).
       01 WORD-8 PIC A(20).
       01 WORD-9 PIC A(20).
       01 WORD-10 PIC A(20).
       01 WORD-11 PIC A(20).
       01 WORD-12 PIC A(20).
       01 WORD-13 PIC A(20).
       01 WORD-14 PIC A(20).
       01 WORD-15 PIC A(20).
       01 WORD-16 PIC A(20).
       01 WORD-17 PIC A(20).
       01 WORD-18 PIC A(20).
       01 WORD-19 PIC A(20).
       01 WORD-20 PIC A(20).
       01 WORD-21 PIC A(20).
       01 WORD-22 PIC A(20).
       01 WORD-23 PIC A(20).
       01 WORD-24 PIC A(20).
       01 WORD-25 PIC A(20).
       01 WORD-26 PIC A(20).
       01 WORD-27 PIC A(20).
       01 WORD-28 PIC A(20).
       01 WORD-29 PIC A(20).
       01 WORD-30 PIC A(20).
       01 WORD-31 PIC A(20).
       01 WORD-32 PIC A(20).
       01 WORD-33 PIC A(20).
       01 WORD-34 PIC A(20).
       01 WORD-35 PIC A(20).
       01 WORD-36 PIC A(20).
       01 WORD-37 PIC A(20).
       01 WORD-38 PIC A(20).
       01 WORD-39 PIC A(20).
       01 WORD-40 PIC A(20).
       01 WORD-41 PIC A(20).
       01 WORD-42 PIC A(20).
       01 WORD-43 PIC A(20).
       01 WORD-44 PIC A(20).
       01 WORD-45 PIC A(20).
       01 WORD-46 PIC A(20).
       01 WORD-47 PIC A(20).
       01 WORD-48 PIC A(20).
       01 WORD-49 PIC A(20).
       01 WORD-50 PIC A(20).
       01 WORD-51 PIC A(20).
       01 WORD-52 PIC A(20).
       01 WORD-53 PIC A(20).
       01 WORD-54 PIC A(20).
       01 WORD-55 PIC A(20).
       01 WORD-56 PIC A(20).
       01 WORD-57 PIC A(20).
       01 WORD-58 PIC A(20).
       01 WORD-59 PIC A(20).
       01 WORD-60 PIC A(20).
       01 WORD-61 PIC A(20).
       01 WORD-62 PIC A(20).
       01 WORD-63 PIC A(20).
       01 WORD-64 PIC A(20).
       01 WORD-65 PIC A(20).
       01 WORD-66 PIC A(20).
       01 WORD-67 PIC A(20).
       01 WORD-68 PIC A(20).
       01 WORD-69 PIC A(20).
       01 WORD-70 PIC A(20).
       01 WORD-71 PIC A(20).
       01 WORD-72 PIC A(20).
       01 WORD-73 PIC A(20).
       01 WORD-74 PIC A(20).
       01 WORD-75 PIC A(20).
       01 WORD-76 PIC A(20).
       01 WORD-77 PIC A(20).
       01 WORD-78 PIC A(20).
       01 WORD-79 PIC A(20).
       01 WORD-80 PIC A(20).
       01 WORD-81 PIC A(20).
       01 WORD-82 PIC A(20).
       01 WORD-83 PIC A(20).
       01 WORD-84 PIC A(20).
       01 WORD-85 PIC A(20).
       01 WORD-86 PIC A(20).
       01 WORD-87 PIC A(20).
       01 WORD-88 PIC A(20).
       01 WORD-89 PIC A(20).
       01 WORD-90 PIC A(20).
       01 WORD-91 PIC A(20).
       01 WORD-92 PIC A(20).
       01 WORD-93 PIC A(20).
       01 WORD-94 PIC A(20).
       01 WORD-95 PIC A(20).
       01 WORD-96 PIC A(20).
       01 WORD-97 PIC A(20).
       01 WORD-98 PIC A(20).
       01 WORD-99 PIC A(20).
       01 WORD-100 PIC A(20).
       01 WORD-101 PIC A(20).
       01 WORD-102 PIC A(20).
       01 WORD-103 PIC A(20).
       01 WORD-104 PIC A(20).
       01 WORD-105 PIC A(20).
       01 WORD-106 PIC A(20).
       01 WORD-107 PIC A(20).
       01 WORD-108 PIC A(20).
       01 WORD-109 PIC A(20).
       01 WORD-110 PIC A(20).
       01 WORD-111 PIC A(20).
       01 WORD-112 PIC A(20).
       01 WORD-113 PIC A(20).
       01 WORD-114 PIC A(20).
       01 WORD-115 PIC A(20).
       01 WORD-116 PIC A(20).
       01 WORD-117 PIC A(20).
       01 WORD-118 PIC A(20).
       01 WORD-119 PIC A(20).
       01 WORD-120 PIC A(20).
       01 WORD-121 PIC A(20).
       01 WORD-122 PIC A(20).
       01 WORD-123 PIC A(20).
       01 WORD-124 PIC A(20).
       01 WORD-125 PIC A(20).
       01 WORD-126 PIC A(20).
       01 WORD-127 PIC A(20).
       01 WORD-128 PIC A(20).
       01 WORD-129 PIC A(20).
       01 WORD-130 PIC A(20).
       01 WORD-131 PIC A(20).
       01 WORD-132 PIC A(20).
       01 WORD-133 PIC A(20).
       01 WORD-134 PIC A(20).
       01 WORD-135 PIC A(20).
       01 WORD-136 PIC A(20).
       01 WORD-137 PIC A(20).
       01 WORD-138 PIC A(20).
       01 WORD-139 PIC A(20).
       01 WORD-140 PIC A(20).
       01 WORD-141 PIC A(20).
       01 WORD-142 PIC A(20).
       01 WORD-143 PIC A(20).
       01 WORD-144 PIC A(20).
       01 WORD-145 PIC A(20).
       01 WORD-146 PIC A(20).
       01 WORD-147 PIC A(20).
       01 WORD-148 PIC A(20).
       01 WORD-149 PIC A(20).
       01 WORD-150 PIC A(20).
       01 WORD-151 PIC A(20).
       01 WORD-152 PIC A(20).
       01 WORD-153 PIC A(20).
       01 WORD-154 PIC A(20).
       01 WORD-155 PIC A(20).
       01 WORD-156 PIC A(20).
       01 WORD-157 PIC A(20).
       01 WORD-158 PIC A(20).
       01 WORD-159 PIC A(20).
       01 WORD-160 PIC A(20).
       01 WORD-161 PIC A(20).
       01 WORD-162 PIC A(20).
       01 WORD-163 PIC A(20).
       01 WORD-164 PIC A(20).
       01 WORD-165 PIC A(20).
       01 WORD-166 PIC A(20).
       01 WORD-167 PIC A(20).
       01 WORD-168 PIC A(20).
       01 WORD-169 PIC A(20).
       01 WORD-170 PIC A(20).
       01 WORD-171 PIC A(20).
       01 WORD-172 PIC A(20).
       01 WORD-173 PIC A(20).
       01 WORD-174 PIC A(20).
       01 WORD-175 PIC A(20).
       01 WORD-176 PIC A(20).
       01 WORD-177 PIC A(20).
       01 WORD-178 PIC A(20).
       01 WORD-179 PIC A(20).
       01 WORD-180 PIC A(20).
       01 WORD-181 PIC A(20).
       01 WORD-182 PIC A(20).
       01 WORD-183 PIC A(20).
       01 WORD-184 PIC A(20).
       01 WORD-185 PIC A(20).
       01 WORD-186 PIC A(20).
       01 WORD-187 PIC A(20).
       01 WORD-188 PIC A(20).
       01 WORD-189 PIC A(20).
       01 WORD-190 PIC A(20).
       01 WORD-191 PIC A(20).
       01 WORD-192 PIC A(20).
       01 WORD-193 PIC A(20).
       01 WORD-194 PIC A(20).
       01 WORD-195 PIC A(20).
       01 WORD-196 PIC A(20).
       01 WORD-197 PIC A(20).
       01 WORD-198 PIC A(20).
       01 WORD-199 PIC A(20).
       01 WORD-200 PIC A(20).
       01 WORD-201 PIC A(20).
       01 WORD-202 PIC A(20).
       01 WORD-203 PIC A(20).
       01 WORD-204 PIC A(20).
       01 WORD-205 PIC A(20).
       01 WORD-206 PIC A(20).
       01 WORD-207 PIC A(20).
       01 WORD-208 PIC A(20).
       01 WORD-209 PIC A(20).
       01 WORD-210 PIC A(20).
       01 WORD-211 PIC A(20).
       01 WORD-212 PIC A(20).
       01 WORD-213 PIC A(20).
       01 WORD-214 PIC A(20).
       01 WORD-215 PIC A(20).
       01 WORD-216 PIC A(20).
       01 WORD-217 PIC A(20).
       01 WORD-218 PIC A(20).
       01 WORD-219 PIC A(20).
       01 WORD-220 PIC A(20).
       01 WORD-221 PIC A(20).
       01 WORD-222 PIC A(20).
       01 WORD-223 PIC A(20).
       01 WORD-224 PIC A(20).
       01 WORD-225 PIC A(20).
       01 WORD-226 PIC A(20).
       01 WORD-227 PIC A(20).
       01 WORD-228 PIC A(20).
       01 WORD-229 PIC A(20).
       01 WORD-230 PIC A(20).
       01 WORD-231 PIC A(20).
       01 WORD-232 PIC A(20).
       01 WORD-233 PIC A(20).
       01 WORD-234 PIC A(20).
       01 WORD-235 PIC A(20).
       01 WORD-236 PIC A(20).
       01 WORD-237 PIC A(20).
       01 WORD-238 PIC A(20).
       01 WORD-239 PIC A(20).
       01 WORD-240 PIC A(20).
       01 WORD-241 PIC A(20).
       01 WORD-242 PIC A(20).
       01 WORD-243 PIC A(20).
       01 WORD-244 PIC A(20).
       01 WORD-245 PIC A(20).
       01 WORD-246 PIC A(20).
       01 WORD-247 PIC A(20).
       01 WORD-248 PIC A(20).
       01 WORD-249 PIC A(20).
       01 WORD-250 PIC A(20).
       01 WORD-251 PIC A(20).
       01 WORD-252 PIC A(20).
       01 WORD-253 PIC A(20).
       01 WORD-254 PIC A(20).
       01 WORD-255 PIC A(20).
       01 WORD-256 PIC A(20).