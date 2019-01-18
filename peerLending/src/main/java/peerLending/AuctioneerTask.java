package peerLending;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import java.io.IOException;
import java.util.*;


public class AuctioneerTask extends TimerTask {
    private String company;
    private String type;
    private Map<String, Company> companies;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;
    private Publisher publisher;

    public AuctioneerTask(String company, Map<String, Company> companies, String type, Map<String, Auction> availableAuctions, Map<String, Emission> availableEmissions, Publisher publisher) {
        this.company = company;
        this.type = type;
        this.companies = companies;
        this.availableAuctions = availableAuctions;
        this.availableEmissions = availableEmissions;
        this.publisher = publisher;
    }


    public void run() {
        if (this.type.equals("Emission")) {
            resultEmission(this.availableEmissions.get(this.company));
        }
        else {
            resultAuction(this.availableAuctions.get(this.company));
        }
    }


    public void resultEmission (Emission emission) {
        Map<String, Integer> subscriptions = emission.getSubscriptions();
        int total = 0;
        //total = subscriptions.values().stream().mapToInt(Integer::value).sum();
        for (Integer amount : subscriptions.values()) {
            total += amount;
        }
        // Notify clients of the result
        String notification = "Emission:"+this.company+":"+emission.getAmount()+":"+emission.getInterest()+":";
        if (total == emission.getAmount()) {
            for (Map.Entry<String, Integer> e : emission.getSubscriptions().entrySet()) {
                notification += e.getKey()+":"+e.getValue()+":";
            }
            notification += "Success";
        }
        else {
            notification += "Failure";
        }
        this.publisher.sendNotification(notification);
        notification = "End" + notification;
        this.publisher.sendNotification(notification);

        sendHTTPRequest("end/emission", emission);
    }

    private static Map<String, Bid> sortByInterest(Map<String, Bid> unsortBids) {

        // Convert Map to List of Map
        List<Map.Entry<String, Bid>> list =
                new LinkedList<Map.Entry<String, Bid>>(unsortBids.entrySet());

        // Sort list with Collections.sort()
        Collections.sort(list, new Comparator<Map.Entry<String, Bid>>() {
            public int compare(Map.Entry<String, Bid> o1,
                               Map.Entry<String, Bid> o2) {
                return ((Float) o1.getValue().getInterest()).compareTo(o2.getValue().getInterest());
            }
        });

        // Loop the sorted list and put it into a new insertion order Map LinkedHashMap
        Map<String, Bid> sortedMap = new LinkedHashMap<String, Bid>();
        for (Map.Entry<String, Bid> entry : list) {
            sortedMap.put(entry.getKey(), entry.getValue());
        }

        return sortedMap;
    }

    public void resultAuction (Auction auction) {
        // Sort bids by interest rate
        Map<String, Bid> bids = sortByInterest(auction.getBids());
        Map<String, Bid> selectedBids = new HashMap<String, Bid>();
        float totalInterest = 0;
        int totalAmount = 0;
        boolean success = false;

        // Maximum interest amount that the company will pay
        float maxInterest = auction.getInterest() * auction.getAmount();

        for (Map.Entry<String, Bid> e : bids.entrySet()) {
            // Bids reached desired amount
            if(totalAmount >= auction.getAmount()){
                success = true;
                break;
            }
            else {
                totalAmount += e.getValue().getAmount();
                totalInterest += e.getValue().getInterest() * e.getValue().getAmount();

                // Maximum interest rate exceeded
                if(totalInterest > maxInterest){
                    break;
                }

                selectedBids.put(e.getKey(), e.getValue());
            }
        }

        // Notify clients of the result
        String notification = "Auction:"+this.company+":"+auction.getAmount()+":"+auction.getInterest()+":";
        if (success) {
            for (Map.Entry<String, Bid> e : selectedBids.entrySet()) {
                notification += e.getKey()+":"+e.getValue().getAmount()+":"+e.getValue().getInterest()+":";
            }
            notification += "Success";
            // Update auction with the actual final bids
            auction.setBids(selectedBids);
        }
        else {
            notification += "Failure";
            auction.setBids(null);
        }
        this.publisher.sendNotification(notification);
        notification = "End" + notification;
        this.publisher.sendNotification(notification);


        // TODO: tenho de enviar a empresa que tem o auction. Como?
        sendHTTPRequest("end/auction", auction);

        // Remove available auction
        this.availableAuctions.remove(this.company);

        // Update company history
        this.companies.get(this.company).putAuction(auction);
    }


    public void sendHTTPRequest(String uri, Object obj){
        CloseableHttpClient httpclient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost("http://localhost:8080/directory/" + uri);
        ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
        String JSON_STRING= null;

        try {
            JSON_STRING = ow.writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }

        HttpEntity stringEntity = new StringEntity(JSON_STRING, ContentType.APPLICATION_JSON);
        httpPost.setEntity(stringEntity);
        try {
            httpclient.execute(httpPost);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
