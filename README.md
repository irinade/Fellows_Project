# Fellows_Project : Analyse of bird sounds

I worked on this project with a fellow classmate. All the detailed informations are present here : https://www.notion.so/SCORE-Project-23f3b4d9849f441db2e5ac4884df237b

You can find here the code and the csv file generated by the code (birds2.csv).
The code also generate spectrograms for each recordings you are studying.
You can also find my "journal de bord" of the 3 weeks I worked on this project

# Counsels for the code

1) Create in your desktop an "Audios" folder with the mp3 recordings you want to analyse.

2) Import to your desktop the "birds.csv" file, when the code will run the code with your recordings, it will add the data to the birds.csv file. Or "initialize" the code by creating a birds.csv file by running the analysis on only one species and saving it in a birds.csv dataframe to generate the first line. 

3) Create in your desktop a "plots" folder, where the code will add the spectrogram of every recording.

4) Do not use to long audio (we tried working with a 15 min recordings, R kept on crashing so we advise not to use long audio). Try not to analyze heavy files (more than 1 or 2 Mo).
