# BSc Thesis Scripts
Το συγκεκριμένο repository περιλαμβάνει όλα τα απαραίτητα csv αρχεία και script της πτυχιακής μου εργασίας, άμεσα εκτελέσιμα σε περιβάλλον R. Για την ομαλή λειτουργία μερικών script, συνιστάται ο ορισμός της περιοχής (region) του συστήματος σε Ηνωμένες Πολιτείες Αμερικής, ώστε οι ημερομηνίες που εκχωρεί η R στα κελιά των data frames να είναι στα Αγγλικά. Εναλλακτικά, στο "about" του repository, δίνεται όλα τα γραφήματα που προκύπτουν από την εκτέλεση του κώδικα με μια συνοδευτική περιγραφή. 

Συνοπτικά, το θέμα της πτυχιακής αφορούσε τη διερεύνηση των απαιτήσεων ενέργειας ηλεκτρικών οχημάτων (PEV) που φορτίζουν σε "έξυπνο" δίκτυο ενέργειας (smart grid) από δεδομένα σε μορφή csv. Στη συνέχεια εφαρμόστηκε η στρατηγική μετατόπησης φορτίων (load shifting), ώστε οι απαιτήσεις ενέργειας του smart grid να μεταφερθούν από τις ώρες αιχμής σε ώρες μη-αιχμής στη διάκρεια της ημέρας. Για τις ανάγκες της εργασίας χρησιμοποιήθηκαν εργαλεία αναλυτικής των δεδομένων (data analytics) και περιγραφικής στατιστικής (descriptive statistics) για τη διαχείριση των δεδομένων και την οπτικοποίηση των αποτελεσμάτων.

## Τα δεδομένα
House.csv: a file containing the household power demand (in watt).\
PEV_L1.csv: a file containing the power demand of PEV charging using Level 1 charging (in watt).\
PEV_L2.csv: a file containing the power demand of PEV charging using Level 2 charging (in watt).\
TimeZones.csv:
LoadShifting.csv

## Τα αρχεία R που δημιουργήθηκαν
"Chapter 2 Plots.R" creates the plots found in Chapter 2 of the thesis.\
"Chapter 4 Plots.R" creates the plots found in Chapter 4 of the thesis.\
"Chapter 5 Plots.R" creates the plots found in Chpater 5 of the thesis.\
"Chapter 6 Plots.R" creates the plots found in Chpater 6 of the thesis.\
"TimeZones.R" creates the Time Zones structure.\
"LoadShifting.R" simulates load shifting scenarios on the TimeZones structure.\
"L1-L2.R" converts LoadShifting or TimeZones structures back into the L1 and L2 type structures.
