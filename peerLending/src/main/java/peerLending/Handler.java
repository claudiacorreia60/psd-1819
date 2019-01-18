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


public class Handler implements Runnable {
    private long startupTime;
    private String company;
    private String type;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;
    private Publisher publisher;

    public Handler (long startupTime, String company, String type, Map<String, Auction> availableAuctions, Map<String, Emission> availableEmissions, Publisher publisher) {
        this.startupTime = startupTime;
        this.company = company;
        this.type = type;
        this.availableAuctions = availableAuctions;
        this.availableEmissions = availableEmissions;
        this.publisher = publisher;
    }


    public void run() {
        while (System.currentTimeMillis() - this.startupTime < 30000);
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
        String notification = "Emission:"+this.company+":"+emission.getAmount()+":"+emission.getInterest()+":";
        if (total == emission.getAmount()) {
            for (Map.Entry<String, Integer> e : emission.getSubscriptions().entrySet()) {
                notification += e.getKey()+":"+e.getValue()+":";
            }
            notification += "Success:End";
        }
        else {
            notification += "Failure:End";
        }
        this.publisher.sendNotification(notification);

        sendHTTPRequest("end/emission", emission);
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

/*    private static Map<String, Bid> sortByInterest(Map<String, Bid> unsortBids) {

        // Convert Map to List of Map
        List<Map.Entry<String, Bid>> list =
                new LinkedList<Map.Entry<String, Bid>>(unsortBids.entrySet());

        // Sort list with Collections.sort()
        Collections.sort(list, new Comparator<Map.Entry<String, Bid>>() {
            public int compare(Map.Entry<String, Bid> o1,
                               Map.Entry<String, Bid> o2) {
                return (o1.getValue().getInterest()).compareTo(o2.getValue().getInterest());
            }
        });

        // 3. Loop the sorted list and put it into a new insertion order Map LinkedHashMap
        Map<String, Integer> sortedMap = new LinkedHashMap<String, Integer>();
        for (Map.Entry<String, Integer> entry : list) {
            sortedMap.put(entry.getKey(), entry.getValue());
        }*/

        /*
        //classic iterator example
        for (Iterator<Map.Entry<String, Integer>> it = list.iterator(); it.hasNext(); ) {
            Map.Entry<String, Integer> entry = it.next();
            sortedMap.put(entry.getKey(), entry.getValue());
        }*/


        //return sortedMap;
    //}

    public void resultAuction (Auction auction) {
        Map<String, Bid> bids = auction.getBids();
        /* TODO: Tratar do algoritmo de escolha das melhores bids */

        // sendHTTPRequest("end/auction", auction);
    }


}
