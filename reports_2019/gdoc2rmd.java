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
        System.out.println("What is the name of the bibliography you'd like to use?"); 
        String references = console.nextLine(); 
        String outputFile = "gdoc2rmd_out.rmd";
        
        //Create a scanner over the given input file, and set up the output file. 
        Scanner fileScan = new Scanner(new File(inFile));
        PrintStream output = new PrintStream(new File(outputFile)); 
        System.setOut(output); 
        
        //Write the YAML header and process the file. 
        writeYamlHeader(title, author, references); 
        processFile(fileScan, dict);            
        output.close(); 
    }
    
    //Build up a dictionary of keywords to convert to formatted 
    // R-markdown text. 
    public static Map<String, keyCounter> makeDictionary(){
      Map<String, keyCounter> dict = new HashMap<String, keyCounter>(); 
      
      //Build up dictionary, starting every count at 0. 
      //First, make generic counters. 
      keyCounter chapterCount = new keyCounter("CHAPTER", 0, "#");
      keyCounter sectionCount = new keyCounter("Section", 0, "##");
      keyCounter figureCount = new keyCounter("Figure", 0, ""); 
      keyCounter annexCount = new keyCounter("Annex", 0, "##"); 
      keyCounter tableCount = new keyCounter("Table", 0, "##"); 
      
      //Then, add every possible variation of key. 
      dict.put("Chapter", chapterCount);
      dict.put("Chapter:", chapterCount); 
      dict.put("chapter", chapterCount); 
      dict.put("chapter:", chapterCount);
      dict.put("CHAPTER", chapterCount); 
      dict.put("CHAPTER:", chapterCount);  
       
      dict.put("Section", sectionCount); 
      dict.put("Section:", sectionCount); 
      dict.put("section", sectionCount); 
      dict.put("section:", sectionCount);
      dict.put("SECTION", sectionCount);
      dict.put("SECTION:", sectionCount);
      
      dict.put("Figure", figureCount);
      dict.put("Figure:", figureCount); 
      dict.put("figure", figureCount); 
      dict.put("figure:", figureCount); 
      dict.put("FIGURE", figureCount); 
      dict.put("FIGURE:", figureCount);
      
      dict.put("ANNEX", annexCount); 
      dict.put("ANNEX:", annexCount); 
      dict.put("Annex", annexCount); 
      dict.put("Annex:", annexCount); 
      dict.put("annex", annexCount); 
      dict.put("annex:", annexCount);  
      
      dict.put("TABLE", tableCount); 
      dict.put("TABLE:", tableCount); 
      dict.put("Table", tableCount); 
      dict.put("Table:", tableCount); 
      dict.put("table", tableCount); 
      dict.put("table:", tableCount); 
      return(dict);
    }
    
    
    //Accepts a Scanner over a file, and formats it in markup language.  
    public static void processFile(Scanner fileScan, Map<String, keyCounter> dict){
      while(fileScan.hasNextLine()){ //EMILY IF KEY == COUNTER, INSTANTIATE A NEW CHILD SECTION CLASS TO RESTART NUMBERING. 
         String line = fileScan.nextLine(); //Get the next line of input
         String lineArray[] = line.split(" ", 2); //Split it into two parts, a "key" and the rest of the line. 
         String key = lineArray[0]; 
         Set<String> allKeys = dict.keySet(); //Get the set of all keys from your dictionary. 
         keyCounter counter = dict.get(key);  
   	   if	(findHeaderKeys(key, allKeys) && counter.getKey().equals("Figure")){ //If this is a figure, add "centering" markup text.  
            System.out.println("<center>"); 
            updateCount(dict, key); 
            System.out.print(convertHeaderKeys(key, allKeys, dict)); 
            if (lineArray.length > 1){
               System.out.println(lineArray[1]); 
            }
            System.out.println("</center>"); 
         } else if (findHeaderKeys(key, allKeys)){ //If the key is one you've seen before, print its' markup text. 
   			System.out.print(convertHeaderKeys(key, allKeys, dict));	//Find the key and convert it if needed. 
            if (lineArray.length > 1){
               System.out.println(lineArray[1]); 
            }
         } else { //If there are no keys, just print the line. 
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
         updateCount(dict, key); 
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
    public static void writeYamlHeader(String title, String author, String references){
      System.out.println("---"); 
      System.out.println("title: \"" + title + "\""); 
      System.out.println("author: \"" + author + "\""); 
      System.out.println("bibliography: " + references); 
      //System.out.println("date: \"May 16, 2019\""); //Do we need date? 
      System.out.println("output: pdf_document"); 
      System.out.println("---"); 
      System.out.println("```{r setup, include=FALSE}"); 
      System.out.println("```"); 
    }

}