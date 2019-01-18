package peerLending;

import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.util.Map;


public class Handler implements Runnable {
    private long startupTime;
    private Object object;
    private Map<String, Object> available;
    // private Publisher publisher ou private ZMQ.Socket publisher ?

    public Handler (long startupTime, Object object) {
        this.startupTime = startupTime;
        this.object = object;
    }


    public void run() {
        Emission emission = null;
        Auction auction = null;
        while (System.currentTimeMillis() - this.startupTime < 30000);
        if (this.object.getClass().getName().equals("Emission")) {
            resultEmission(emission);
        }
        else {
            resultAuction(auction);
        }
    }



    public void resultEmission (Emission emission) {
        Map<String, Integer> subscriptions = emission.getSubscriptions();
        int total = 0;
        //total = subscriptions.values().stream().mapToInt(Integer::value).sum();
        for (Integer amount : subscriptions.values()) {
            total += amount;
        }
        if (total == emission.getAmount()) {
            /* TODO: Enviar notificação a cada investor do map subscriptions e à company */
        }
    }

    public void resultAuction (Auction auction) {
        Map<String, Bid> bids = auction.getBids();
        /* TODO: Tratar do algoritmo de escolha das melhores bids */
    }


}
