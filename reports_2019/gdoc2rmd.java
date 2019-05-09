// Prompts the user for a file name to convert from Google Doc report formatting 
// to R Markdown formatting. 

//Rules: 
// "Header1" = #
// "Header2" = ## 
// "Header3" = ### 
// "Header4" = ####
// "Header5" = #####
// "Header6" = ######

import java.io.*;
import java.util.*;

public class gdoc2rmd {
   
    public static void main(String[] args) throws IOException {
        System.out.println("This program decodes a file with a Huffman code.");
        System.out.println();
        
        //EMILY CAN YOU WRITE A YAML HEADER DYNAMICALLY ??? 

        Scanner console = new Scanner(System.in);
        System.out.print("What is the name of the plain text file you'd like to convert? ");
        //Need to test if this is actually a file. 
        String inFile = console.nextLine();
        String outputFile = "gdoc2rmd_out.txt";
        
        // open code file and construct tree
        Scanner fileScan = new Scanner(new File(inFile));
                    
        System.out.print("Output created: 'gdoc2rmd_out.txt'"); 
    }
    
    //Accepts a Scanner over a file, and formats it in 
    // the markup language we've selected. 
    public static void processFile(Scanner fileScan){
      while(fileScan.hasNextLine()){
         String line = fileScan.nextLine(); 
         Scanner lineScan = new Scanner(line); 
         String key = lineScan.next(); 
         if (findHeaderKeys(key)){
            System.out(convertHeaderKeys(key)); 
            while (lineScan.hasNext()){
               System.out(lineScan.next()); 
            }
         } else {
            System.out(line); 
         }
      }

      //For each line in the file, pull out the first bit. 
      // See if it's a key for anything. 
      // If it's a key, convert it and print the line. 
      // If it's not a key, print the line. 
      //Fin. 
    }
    
    public static boolean findHeaderKeys(){
      return(true);  
    }
    
    public static String convertHeaderKeys(){
    }
    
    
    //Emily do you want a function here to convert headers? 

}