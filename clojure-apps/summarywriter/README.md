# Summary Writer

This is a real app my Dad wrote, he converted it to a teaching exercise.

## Set up your project using Leiningen

In core.clj:



    (ns survey-report.core

      (:gen-class))



    (require '[clojure.java.io :as io]

             '[clojure-csv.core :as csv]

             '[semantic-csv.core :as sc]

             '[hiccup.core :as hic])


[semantic-csv](https://www.google.com/url?q=https://github.com/metasoarous/semantic-csv&sa=D&ust=1495472622531000&usg=AFQjCNGGxMqHY79BHadtNSjSNKpH-HAYiQ) (which depends on clojure-csv) will do the CSV processing, but you’ll only need one function: sc/slurp-csv.

## count-occurrences()

*   Look at [this explanation](https://www.google.com/url?q=http://clj-me.cgrand.net/2009/04/27/counting-occurences/&sa=D&ust=1495472622533000&usg=AFQjCNEcPVmn1ZBK12pPSKcU8YzRBjnlsA) of how to count occurrences in a sequence
*   Do your best to understand each step he takes in arriving at the final form
*   play with each step in the REPL
*   write a function count-occurrences() that takes a sequence and returns a map with items together with number of times the item appears in the sequence (basically take his expression and wrap it in a function)



## responses

*   Survey responses come in a CSV file, one row per response
*   Use sc/slurp-csv in the REPL to load the CSV file into a variable responses, look at the first few items on the list to see what a survey response looks like when loaded



## collect-answers()

*   Each survey question has a q-code associated with it, e.g. Q209S1 (corresponds to column in the CSV file)
*   Each survey response in responses is a map, where the key is a q-code for a question and the value is the answer to the question
*   Write a function collect-answers which takes two arguments, a list of responses and a q-code, and returns a list containing the corresponding answer from each survey response … essentially, the contents of that particular column of the CSV file
*   Oops! Some of the questions weren’t answered, in which case the value is an empty string. Modify your function to filter these out of the answer list.



## collect-answers-multi()

*   A few of the survey questions are made up of multiple questions, e.g. “What three things did you like best?” is actually three questions with q-codes Q4S1, Q4S2, Q4S3\. So when collecting answers into a list we want to be able to add answers for multiple q-codes.
*   Write a function collect-answers-multi which takes two arguments, a list of responses and a sequence of q-codes, and uses collect-answers to get the answers for each q-code, then puts the answers together into a single list. (Hint: use clojure function mapcat)



## choice-totals()

*   Many of the questions on the survey have multiple-choice answer values, like "Xpos" "Vpos" "Pos" "Neg" "VNeg" or "Alway" "Usual" "Somet" "Rarel". We’ll be reporting these as a row of numbers in a table, i.e. how many students answered always, usually, etc. so we need to total up the different answer values given for each question.
*   Write a function choice-totals which takes two arguments, a list of responses and a q-code, and returns a map with key being an answer given and value being the number of times it was given. (Hint: count-occurences() can do this for a list of answers!)



## choice-row()

*   For each q-code with multiple choice answers we’ll display the answers as a row in an HTML table.
*   Write a function choice-row which takes four arguments--a list of responses, a row label, a map of choices with key being an answer given and value being the number of times it was given (i.e. produced by choice-totals()), and a q-code--and creates a Hiccup structure representing the HTML table row, table cells, and values needed to display the answers.
*   You’ll need to look at how the [Hiccup library works](https://www.google.com/url?q=https://github.com/weavejester/hiccup&sa=D&ust=1495472622552000&usg=AFQjCNG9IY-y-gwbKsB8vVAU84qj8QxVVg). It’s very straightforward, HTML tag pairs are represented by a vector where the first element is a keyword corresponding to the tag, e.g. :span, :div, and the remaining elements are what falls between the tags.
*   Example: “<html><head><title>A document</title></head><body>Some text</body></html>” would be:

        [:html
        [:head
            [:title “A document”]]
        [:body “Some text”]]]

*   All of your functions will build up/manipulate these structures, only turning them in to HTML once the entire document is built. But you can turn any Hiccup snippet into an HTML snippet (string) using hic/html(). While I was writing these functions I would produce snippets and paste them into a CodePen project to see how they looked. You could also write a utility function that would take a Hiccup structure, convert it to an HTML string, and write it to a file that you could then view in a browser.



## choice-matrix()

*   Multiple choice survey questions come in groups, where one or more questions has the same set of answers (e.g. “Xpos" "Vpos" "Pos" "Neg" "VNeg"). These need to be displayed as a table with the titles across the top (i.e. column headers), followed by rows displaying the answer counts for each possible answer.
*   Write a function choice-matrix which takes four arguments--a list of responses, a vector of column titles, a vector of choices, and a list of row specs (see below), and returns a Hiccup structure which is a table where the first row contains the column titles (first entry is blank, since it will be used for the question labels) and the subsequent rows contain the question label followed by the answer values for the question in a row spec
*   A row spec is a map that specifies the format of a row. For now it only contains values, :label (specifies the string that appears in the first column) and :q-code (specifies the q-code to use to get results for the row)



## answer-list()

*   Used for questions with textbox answers, this will display them in an unordered list with a header
*   Write a function answer-list that takes three arguments--a list of responses, a header, and either a q-code or a vector of q-codes--and returns a Hiccup structure representing a div that contains two elements, an H3 header followed by an unordered list of strings that are either (a) the answers to the question indicated by the q-code, or (b) a combined list of answers to the questions indicated by the vector of q-codes



## choice-list()

*   A few questions have multiple-choice answers that we want to display as a list rather than a table row, e.g. “Where did you first hear about the class?” has 14 possible answers.
*   Write a function answer-list that takes four arguments--a list of responses, a header, a q-code, and a vector of choice specs (see below)--and returns a Hiccup structure representing a div that contains two elements, an H3 header followed by an unordered list of strings built from the answer choices specified by the choice specs.
*   A choice spec is a map that specifies the full name of each choice--:choice-code specifies the answer as found in the response, and :label the corresponding full text.
*   The format of each list item should be “Full choice name (n)”, where n is the number of responses that chose that option



## teacher-comment()

*   For each teacher/assistant the survey asked two level-of-satisfaction multiple choice questions (“Fun?” and “Results?”) plus one “Additional comments” question.
*   The three questions had related q-codes--the two multiple choice questions ended in “S1” and “S2”, while the other question ended in “a”--e.g. for a teacher with partial q-code “Q211” the three full q-codes would be “Q211S1”, “Q211S2”, “Q211a”
*   Write a function teacher-comment which takes three arguments--a list of responses, a teacher name, and a partial q-code--and returns a Hiccup structure representing a div containing four elements: a horizontal rule, an h3 containing the teacher name, a choice matrix for questions “Fun?” and “Results?”, and an (unlabeled) answer list for additional comments.
