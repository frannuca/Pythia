package fjn.pythia.networking.wcfServer;

import javax.jws.WebMethod;
import javax.jws.WebService;
import javax.xml.ws.Endpoint;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/31/11
 * Time: 4:04 PM
 * To change this template use File | Settings | File Templates.
 */
//@WebService
public class pythiaWCFServer {
    public static void main(String[ ] args) {
    int port = 8888;
    String url = "http://localhost:" + port + "/nurbs";
    System.out.println("Publishing Teams on port " + port);
    Endpoint.publish(url, new NurbsService());
    }
}
