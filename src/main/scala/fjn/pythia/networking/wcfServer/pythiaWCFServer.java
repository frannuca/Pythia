package fjn.pythia.networking.wcfServer;

import fjn.pythia.networking.messages.Job;
import fjn.pythia.networking.messages.ResourceSpec;

import javax.annotation.Resource;
import javax.jws.WebMethod;
import javax.jws.WebService;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/31/11
 * Time: 4:04 PM
 * To change this template use File | Settings | File Templates.
 */
@WebService
public class pythiaWCFServer {
    @WebMethod
    public byte[] ProcessCommonDataMessage(byte[] msg) throws Exception{
        throw new Exception("Not implemented");
    }

    @WebMethod
    public int registerResource(ResourceSpec resource) throws Exception{
        throw new Exception("Not implemented");
    }

    @WebMethod
    public List<ResourceSpec> getResources() throws Exception{

        throw new Exception("Not implemented");
    }

    @WebMethod
    public int SubmitJob(Job job) throws Exception
    {
        throw new Exception("Not implemented");
    }



}
