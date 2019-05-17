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
        //Build up a dictionary to convert certain character strings into R markdown code. 
        HashMap<String, String> dict = makeDictionary();     
        Set<String> allKeys = dict.keySet(); 
        
        Scanner console = new Scanner(System.in);
        System.out.print("What is the name of the plain text file you'd like to convert? ");
        //Need to test if this is actually a file. 
        String inFile = console.nextLine();
        String outputFile = "gdoc2rmd_out.rmd";
        
        // open code file and construct tree
        Scanner fileScan = new Scanner(new File(inFile));
        PrintStream output = new PrintStream(new File(outputFile)); 
        System.setOut(output); 
        processFile(fileScan);            
        output.close(); 
    }
    
    public static HashMap<String, String> makeDictionary(){
      HashMap<String, String> dict = new HashMap<String, String>(); 
      
      //Build up dictionary - add new conversions here. 
      dict.put("Chapter", "##"); 
      return(dict);
    }
    
    //Accepts a Scanner over a file, and formats it in 
    // the markup language we've selected. 
    public static void processFile(Scanner fileScan){
      writeYamlHeader(); 
      while(fileScan.hasNextLine()){
         System.out.println(); 
         String line = fileScan.nextLine(); 
         String lineArray[] = line.split(" ", 2); 
         String key = lineArray[0]; 
   	   if	(findHeaderKeys(key, allKeys)){
   			System.out.print(convertHeaderKeys(key));	//Find the key and convert it if needed. 
            if (lineArray.length > 1){
               System.out.print(lineArray[1]); 
            }
         } else {
      	   System.out.println(line);	
         }
      }

    }
    
    //If the header is a key, convert it to its corresponding 
    // markdown code. 
    public static boolean findHeaderKeys(String key, Set<String> allKeys){
      if (allKeys.contains(key)){
         return(true); 
      }  
      return(false); 
    }
    
    public static String convertHeaderKeys(String key){
      if (allKeys.contains(key)){
         return(dict.get(key) + " "); 
      }
      return(key + " "); 
    }
    
    public static void writeYamlHeader(){
      System.out.println("---"); 
      System.out.println("title: \"Untitled\""); 
      System.out.println("author: \"IHME PCE\""); 
      System.out.println("date: \"May 16, 2019\"");
      System.out.println("output: pdf_document"); 
      System.out.println("---"); 
      System.out.println("```{r setup, include=FALSE}"); 
      System.out.println("```"); 
    }
    //Emily do you want a function here to convert headers? 

}