package ycharts;
import java.io.File;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;

public class Ychart_gettabledata {
	
	public static void main(String[]args) throws InterruptedException {
		
	
	//	File file = new File("driver/chromedriver.exe");
		System.setProperty("webdriver.chrome.driver", "/Users/tinapraveen/Downloads/chromedriver");
		WebDriver driver = new ChromeDriver();
		driver = new ChromeDriver();
		driver.manage().window().maximize();
		driver.get("https://ycharts.com/login?next=%2Fcompanies%2FAAPL%2Fpe_ratio"); 
		driver.findElement(By.xpath("//input[@name='username']")).sendKeys("Shyama.l.kanth@gmail.com");
	    driver.findElement(By.xpath("//input[@name='password']")).sendKeys("S12345678a");
	    driver.findElement(By.xpath("//button[@class='btn']")).click();
	    Thread.sleep(1000);
	    
	    driver.manage().timeouts().implicitlyWait(30, TimeUnit.SECONDS);
    
	    WebElement fromDate = driver.findElement(By.xpath("//input[@ng-model='startDate']"));
	    fromDate.clear();
	    fromDate.sendKeys("01/01/2018");
	    
	    WebElement toDate = driver.findElement(By.xpath("//input[@ng-model='endDate']"));
	    toDate.clear();
	    toDate.sendKeys("03/26/2018");
	    
	    WebElement getData = driver.findElement(By.xpath("//input[@value='Get Data']"));
	    getData.click();
	    
	    
	    
	    String leftfirst_part = "//div[@class='ng-binding']//div[@class='dataColLeft']/div/table/tbody/tr[";
	    String leftsecond_part = "]/td[";
	    String leftthird_part = "]";
	    
	    
	    
	    String rightfirst_part = "//div[@class='ng-binding']//div[@class='dataColRt']/div/table/tbody/tr[";
	    String rightsecond_part = "]/td[";
	    String rightthird_part = "]";
	    
	    
	    
	    WebElement text = driver.findElement(By.xpath("//h2[text()='Apple Historical PE Ratio (TTM) Data']"));
	    
	    while(text.isDisplayed()){
	    	
	    	List<WebElement> leftrow = driver.findElements(By.xpath("//div[@class='ng-binding']//div[@class='dataColLeft']/div/table/tbody/tr"));
	        int leftRow_count = leftrow.size();
	       //System.out.println("Number Of Rows in left table = "+leftRow_count);
	        
	        List<WebElement> rightrow = driver.findElements(By.xpath("//div[@class='ng-binding']//div[@class='dataColRt']/div/table/tbody/tr"));
	        int rightRow_count = rightrow.size();
	       // System.out.println("Number Of Rows in right table = "+rightRow_count);
	        
	        List<WebElement> col = driver.findElements(By.xpath("//div[@class='ng-binding']//div[@class='padR']/table/tbody/tr[2]/td"));
	        int Col_count = col.size();
	       // System.out.println("Number Of Columns = "+Col_count);
	        Thread.sleep(1000);
	        for(int i=2; i<=leftRow_count; i++){
	        	for(int j=1; j<=Col_count; j++){
	        		
	        		String leftfinal_xpath = leftfirst_part+i+leftsecond_part+j+leftthird_part;
        		   
        		   String leftTable_data = driver.findElement(By.xpath(leftfinal_xpath)).getText();
        		   System.out.print(leftTable_data +"  "); 
        		   
	        	}
	        	
	        	System.out.println("");
	            }
	        
	         for(int i=2; i<=rightRow_count; i++){
	        	 
	        	 for(int j=1; j<=Col_count; j++){
	        		 
	        		 String rightfinal_xpath = rightfirst_part+i+rightsecond_part+j+rightthird_part;   
	     		     String rightTable_data = driver.findElement(By.xpath(rightfinal_xpath)).getText();
	     		     System.out.print(rightTable_data +"  "); 
	     		     }
	        	 System.out.println(""); 
	        	 
	        	 }
	         WebElement btnNext = driver.findElement(By.xpath("//a[@class='nextBtn']"));
	         
	         btnNext.click();
	       
	         Thread.sleep(1000);
	         
	         }
	}
	
}


