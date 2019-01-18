package peerLending;

import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.util.Map;


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
    }

    public void resultAuction (Auction auction) {
        Map<String, Bid> bids = auction.getBids();
        /* TODO: Tratar do algoritmo de escolha das melhores bids */
    }


}
