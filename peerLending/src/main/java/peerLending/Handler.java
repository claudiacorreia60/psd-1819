package peerLending;

import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;


public class Handler implements Runnable {
    private long startupTime;
    private Object object;
    private InputStream in;
    private OutputStream out;
    // private Publisher publisher ou private ZMQ.Socket publisher ?

    public Handler (long startupTime, Object object, InputStream in, OutputStream out) {
        this.startupTime = startupTime;
        this.object = object;
        this.in = in;
        this.out = out;
    }


    public void run() {
        Emission emission = null;
        Auction auction = null;
        try {
            while (System.currentTimeMillis() - this.startupTime < 30000) {
                if (this.object.getClass().getName().equals("Emission")) {
                    emission = (Emission) this.object;
                    listenSubscriptions(emission);
                }
                else {
                    auction = (Auction) this.object;
                    listenBids(auction);
                }
            }
            if (this.object.getClass().getName().equals("Emission")) {
                resultEmission(emission);
            }
            else {
                resultAuction(auction);
            }
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void listenSubscriptions (Emission emission) throws IOException {
        byte [] response = this.receive();
        ClientProtos.Message msg = ClientProtos.Message.parseFrom(response);
        if (msg.getType().equals("Subscription")) {
            Map<String, Integer> subscriptions = emission.getSubscriptions();
            int total = 0;
            //total = subscriptions.values().stream().mapToInt(Integer::value).sum();
            for (Integer amount : subscriptions.values()) {
                total += amount;
            }
            if ((total + msg.getAmount()) <= emission.getAmount()) {
                subscriptions.put(msg.getInvestor(), msg.getAmount());
                emission.setSubscriptions(subscriptions);
            }
            else {
                ClientProtos.Result result = ClientProtos.Result.newBuilder()
                        .setResult(false)
                        .setEntity("investor")
                        .build();
                ClientProtos.Message reply = ClientProtos.Message.newBuilder()
                        .setRes(result)
                        .setInvestor(msg.getInvestor())
                        .build();
                this.out.write(reply.toByteArray());
                this.out.flush();
            }
        }
    }

    public void listenBids (Auction auction) throws InvalidProtocolBufferException {
        byte [] response = this.receive();
        ClientProtos.Message msg = ClientProtos.Message.parseFrom(response);
        if (msg.getType().equals("Bid")) {
            Map<String, Bid> bids = auction.getBids();
            Bid bid = new Bid(msg.getInvestor(), msg.getAmount(), msg.getInterest());
            bids.put(msg.getInvestor(), bid);
            auction.setBids(bids);
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

    public byte[] receive(){
        byte[] tmp = new byte[4096];
        int len = 0;
        try {
            len = this.in.read(tmp);
            byte[] response = new byte[len];

            for(int i = 0; i < len; i++)
                response[i] = tmp[i];
            return response;
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

}
