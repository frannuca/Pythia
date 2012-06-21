//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2012.06.02 at 11:37:56 AM CEST 
//


package fjn.pythia.networking.message.bindings;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for IVector complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="IVector">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="value" type="{http://www.w3.org/2001/XMLSchema}integer" maxOccurs="unbounded"/>
 *         &lt;element name="dummyFriend" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "IVector", propOrder = {
    "value",
    "dummyFriend"
})
public class IVector {

    @XmlElement(required = true)
    protected List<BigInteger> value;
    protected String dummyFriend;

    /**
     * Gets the value of the value property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the value property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getValue().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BigInteger }
     * 
     * 
     */
    public List<BigInteger> getValue() {
        if (value == null) {
            value = new ArrayList<BigInteger>();
        }
        return this.value;
    }

    /**
     * Gets the value of the dummyFriend property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDummyFriend() {
        return dummyFriend;
    }

    /**
     * Sets the value of the dummyFriend property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDummyFriend(String value) {
        this.dummyFriend = value;
    }

}
