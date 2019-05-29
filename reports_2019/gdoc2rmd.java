// Prompts the user for a file name to convert from Google Doc report formatting 
// to R Markdown formatting. 

import java.io.*;
import java.util.*;

public class gdoc2rmd { 
   
    public static void main(String[] args) throws IOException {   
        //Build up a dictionary of "keys" and their given counts.  
        Map<String, keyCounter> dict = makeDictionary();      
        
        //Take user input to set up the file. 
        Scanner console = new Scanner(System.in);
        System.out.println("What is the name of the plain text file you'd like to convert? ");
        // *Need to test if this is actually a file. 
        String inFile = console.nextLine();
        System.out.println("What is the title of your document?"); 
        String title = console.nextLine(); 
        System.out.println("Who is the author of your document?"); 
        String author = console.nextLine(); 
        String outputFile = "gdoc2rmd_out2.rmd";
        
        //Create a scanner over the given input file, and set up the output file. 
        Scanner fileScan = new Scanner(new File(inFile));
        PrintStream output = new PrintStream(new File(outputFile)); 
        System.setOut(output); 
        
        //Write the YAML header and process the file. 
        writeYamlHeader(title, author); 
        processFile(fileScan, dict);            
        output.close(); 
    }
    
    //Build up a dictionary of keywords to convert to formatted 
    // R-markdown text. 
    public static Map<String, keyCounter> makeDictionary(){
      Map<String, keyCounter> dict = new HashMap<String, keyCounter>(); 
      
      //Build up dictionary - add new conversions here. 
      //Start every counter at 0. 
      dict.put("Chapter", new keyCounter("Chapter", 0, "###")); 
      dict.put("Section", new keyCounter("Section", 0, "##")); 
      dict.put("Figure:", new keyCounter("Figure", 0, "")); 
      return(dict);
    }
    
    
    //Accepts a Scanner over a file, and formats it in 
    // the markup language we've select(key + " " + incrementCount(key, counter) + " ")ed. 
    public static void processFile(Scanner fileScan, Map<String, keyCounter> dict){
      while(fileScan.hasNextLine()){
         System.out.println(); 
         String line = fileScan.nextLine(); 
         String lineArray[] = line.split(" ", 2); 
         String key = lineArray[0]; 
         Set<String> allKeys = dict.keySet(); 
   	   if	(findHeaderKeys(key, allKeys) & !key.equals("Figure:")){
   			System.out.print(convertHeaderKeys(key, allKeys, dict));	//Find the key and convert it if needed. 
            if (lineArray.length > 1){
               System.out.print(lineArray[1]); 
            }
         } else if (key.equals("Figure:")){
            System.out.println("<center>"); 
            updateCount(dict, key); 
            keyCounter counter = dict.get(key);  
            System.out.print("Figure " + counter.getCount() + ": "); 
            if (lineArray.length > 1){
               System.out.println(lineArray[1]); 
            }
            System.out.println("</center>"); 
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
    public static String convertHeaderKeys(String key, Set<String> allKeys, Map<String, keyCounter> dict){
      if (allKeys.contains(key)){
         return(dict.get(key) + " "); 
      }
      updateCount(dict, key); 
      keyCounter counter = dict.get(key); 
      return(key + " " + counter.getCount() + " "); 
    }
    
    //Update the count after calling a certain key in the dictionary. 
    public static Map<String, keyCounter> updateCount(Map<String, keyCounter> dict, String key){
      keyCounter counter = dict.get(key); 
      counter.add(); 
      dict.put(key, counter); 
      return(dict); 
    } 
    
    //Write a standard YAML header. 
    public static void writeYamlHeader(String title, String author){
      System.out.println("---"); 
      System.out.println("title: \"" + title + "\""); 
      System.out.println("author: \"" + author + "\""); 
      //System.out.println("date: \"May 16, 2019\""); //Do we need date? 
      System.out.println("output: pdf_document"); 
      System.out.println("---"); 
      System.out.println("```{r setup, include=FALSE}"); 
      System.out.println("```"); 
    }

}