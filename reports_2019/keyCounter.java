public class keyCounter{
   
   private String key; 
   private int count; //Keeps track of the number of times a key has been called.
   private String markup; //The HTML markup associated with this key. 
 
   //Constructs a keyCounter with default values. 
   public keyCounter(String key){
      this(key, 0, "");
   }
   
   //Constructs a keyCounter with given values for count and HTML markup. 
   public keyCounter(String key, int count, String markup){
      this.key = key; 
      this.count = count; 
      this.markup = markup;  
   }
   
   //Including a toString method for printing. 
   public String toString(){
      return(this.markup + " " + this.key + " " + this.count + ": "); 
   } 
   
   //Returns this class's key. 
   public String getKey(){
      return(this.key); 
   } 
   
   //Returns this key's count. 
   public int getCount(){
      return(this.count);       
   } 
   
   //Returns this key's HTML markup. 
   public String getMarkup(){
      return(this.markup); 
   } 

   //Increments this key's count by 1. 
   public void add(){
      this.count++; 
   }
   
}


