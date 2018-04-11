package ycharts;
//import java.io.File;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;

public class Ychart_gettabledata {
	

	public static void main(String[]args) throws InterruptedException, FileNotFoundException {
		
		
		String[] companies= {"OXY","COP","MRO"};
		String[] variables= {"market_cap","pe_ratio","ps_ratio","ev_ebit","operating_earning_yield","peg_ratio","earning_yield","price_to_book_value","ev_revenues","dividend_yield","cash_dividend_payout_ratio_annual","profit_margin_ttm","payout_ratio","gross_profit_margin_ttm","return_on_invested_capital","return_on_equity","debt_equity_ratio_annual","fulmer_h_score","book_value_of_equity","current_ratio","high_price","low_price","volume","days_sales_outstanding","days_payables_outstanding","receivables_turnover_ttm","return_on_assets","return_on_invested_capital","quick_ratio","eps_est_long_term_growth"}; 
		for(int j=1;j<3;j++)
			for(int i=0;i<30;i++)
			generate_csv(companies[j],variables[i]);
	}
	
	static void generate_csv(String com,String var) throws InterruptedException, FileNotFoundException
	{
		String login_str="https://ycharts.com/login?next=%2Fcompanies%2F"+com+"%2F"+var;
		System.setProperty("webdriver.chrome.driver", "/Users/ishwaryachemarthi/eclipse-workspace/Project1/chromedriver");
		WebDriver driver = new ChromeDriver();
		driver = new ChromeDriver();
		driver.manage().window().maximize();
		driver.get(login_str);
		if(driver.findElements(By.xpath("//input[@name='username']")).size()==0)
		{	
			System.out.println("skipped for"+com+var);
			return;
		}
		driver.findElement(By.xpath("//input[@name='username']")).sendKeys("sbelde2@uic.edu");
	    driver.findElement(By.xpath("//input[@name='password']")).sendKeys("Sai123456");
	    driver.findElement(By.xpath("//button[@class='btn']")).click();
	    Thread.sleep(1000);
	    

	    driver.manage().timeouts().implicitlyWait(30, TimeUnit.SECONDS);
    

	    WebElement fromDate = driver.findElement(By.xpath("//input[@ng-model='startDate']"));
	    fromDate.clear();
	    fromDate.sendKeys("02/01/1998");
	    

	    WebElement toDate = driver.findElement(By.xpath("//input[@ng-model='endDate']"));
	    toDate.clear();
	    toDate.sendKeys("02/01/2018");
	    
	    WebElement getData = driver.findElement(By.xpath("//input[@value='Get Data']"));
	    getData.click();
	    
	    String leftfirst_part = "//div[@class='ng-binding']//div[@class='dataColLeft']/div/table/tbody/tr[";
	    String leftsecond_part = "]/td[";
	    String leftthird_part = "]";
	    
	    String rightfirst_part = "//div[@class='ng-binding']//div[@class='dataColRt']/div/table/tbody/tr[";
	    String rightsecond_part = "]/td[";
	    String rightthird_part = "]";
	    String File_name=com+"_"+var+".csv";
	    PrintWriter pw = new PrintWriter(new File(File_name));
        
	    while(true){
	    	
	    	StringBuilder sb = new StringBuilder();
	    	
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
        		   //System.out.print(leftTable_data +"  "); 
        		   leftTable_data = leftTable_data.replace(",","");
        		   sb.append(leftTable_data);
        		   if(j!=Col_count)
        			   sb.append(",");
        		   else
        			   sb.append("\n");
	        	}
	        	

	        	//System.out.println("");
	            }
	        

	         for(int i=2; i<=rightRow_count; i++){
	        	 

	        	 for(int j=1; j<=Col_count; j++){
	        		 
	        		 	
	        		 	String rightfinal_xpath = rightfirst_part+i+rightsecond_part+j+rightthird_part;   
	     		     	String rightTable_data = driver.findElement(By.xpath(rightfinal_xpath)).getText();
	     		     	//System.out.print(rightTable_data +"  "); 
	     		     	rightTable_data = rightTable_data.replace(",","");
	     		     	sb.append(rightTable_data);
	     		     	if(j!=Col_count)
	         			   sb.append(",");
	         		   else
	         			   sb.append("\n");
	     		     }
	        	 //System.out.println(""); 
	        	 

	        	 }
	         
	         	pw.write(sb.toString());
	         	List<WebElement> btnNext;
		    	if(driver.findElements(By.xpath("//a[@class='nextBtn']")).size()!=0)
		    	{	
		    		btnNext = driver.findElements(By.xpath("//a[@class='nextBtn']"));
		    		btnNext.get(0).click();
		    		Thread.sleep(1000);
		    	}
		    	else
		    		break; 
	       
	         
	         }
        pw.close();
	}
}
