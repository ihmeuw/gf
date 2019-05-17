// Prompts the user for a file name to convert from Google Doc report formatting 
// to R Markdown formatting. 

import java.io.*;
import java.util.*;

public class gdoc2rmd { 
   
    public static void main(String[] args) throws IOException {   
        //Build up a dictionary to convert certain character strings into R markdown code. 
        HashMap<String, String> dict = makeDictionary();    
        HashMap<String, Integer> figureCounter = makeCounter(dict);   
        
        Scanner console = new Scanner(System.in);
        System.out.print("What is the name of the plain text file you'd like to convert? ");
        //Need to test if this is actually a file. 
        String inFile = console.nextLine();
        String outputFile = "gdoc2rmd_out.rmd";
        
        // open code file and construct tree
        Scanner fileScan = new Scanner(new File(inFile));
        PrintStream output = new PrintStream(new File(outputFile)); 
        System.setOut(output); 
        processFile(fileScan, dict, figureCounter);            
        output.close(); 
    }
    
    //Build up a dictionary of keywords to convert to formatted 
    // R-markdown text. 
    public static HashMap<String, String> makeDictionary(){
      HashMap<String, String> dict = new HashMap<String, String>(); 
      
      //Build up dictionary - add new conversions here. 
      //Start every counter at 0. 
      dict.put("Chapter", "##"); 
      return(dict);
    }
    
    //Build up a counter to auto-number figures and chapters from 
    // the given dictionary. 
    public static HashMap<String, Integer> makeCounter(HashMap<String, String> dict){
      HashMap<String, Integer> counter = new HashMap<String, Integer>(); 
      
      //For all the keys that have been added to the dictionary above, 
      // start the count at 0. 
      for (String key:dict.keySet()){
         counter.put(key, 0); 
      } 
      return(counter);
    }
    
    //Accepts a Scanner over a file, and formats it in 
    // the markup language we've selected. 
    public static void processFile(Scanner fileScan, HashMap<String, String> dict, HashMap<String, Integer> counter){
      writeYamlHeader(); 
      while(fileScan.hasNextLine()){
         System.out.println(); 
         String line = fileScan.nextLine(); 
         String lineArray[] = line.split(" ", 2); 
         String key = lineArray[0]; 
         Set<String> allKeys = dict.keySet(); 
   	   if	(findHeaderKeys(key, allKeys)){
   			System.out.print(convertHeaderKeys(key, allKeys, dict, counter));	//Find the key and convert it if needed. 
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
    
    //Convert a given header to its corresponding R markdown code. 
    public static String convertHeaderKeys(String key, Set<String> allKeys, HashMap<String, String> dict, HashMap<String, Integer> counter){
      if (allKeys.contains(key)){
         return(dict.get(key) + " "); 
      }
      return(key + " " + incrementCount(key, counter) + " "); 
    }
    
    //Increment the count for a given header. 
    public static int incrementCount(String key, HashMap<String, Integer> counter){
      int count = counter.get(key);
      counter.put(key, count++); 
      return(count); 
    }
    
    //Write a standard YAML header. 
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

}