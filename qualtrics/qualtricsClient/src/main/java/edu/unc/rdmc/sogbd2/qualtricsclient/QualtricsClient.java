/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */
package edu.unc.rdmc.sogbd2.qualtricsclient;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.entity.StringEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * source: 
 * https://api.qualtrics.com/5e86e383167d5-getting-survey-responses-via-the-new-export-ap-is#using-java-to-get-survey-responses
 * This client uses Apache HttpClient version 4.5 (not 5.x) jars
 */
public class QualtricsClient {
    private static final Logger logger = Logger.getLogger(QualtricsClient.class.getName());
    
    private static final String SURVEY_ID = "";
    private static final String API_TOKEN = "";
    private static final String DATA_CENTER = "";

    private static HttpClient httpClient;
    private static ObjectMapper mapper;

    public static void main(String[] args) throws IOException, URISyntaxException, InterruptedException, Exception {
        
        httpClient = HttpClientBuilder.create().build();
        mapper = new ObjectMapper();

        // Step 1: Start the export
        URI uri = new URIBuilder()
          .setScheme("https")
          .setHost(String.format("%s.qualtrics.com", DATA_CENTER))
          .setPath("/API/v3/surveys/" + SURVEY_ID + "/export-responses")
          .build();

        HttpPost post = new HttpPost(uri);
        post.setHeader("X-API-TOKEN", API_TOKEN);
        post.setHeader("Content-Type", "application/json");

        //StringEntity body = new StringEntity(String.format("{ \"format\": \"csv\" }"));
        StringEntity body = new StringEntity(String.format("{ \"format\": \"csv\", \"useLabels\" : \"TRUE\" }"));
        post.setEntity(body);
        JsonNode responseJson = sendRequest(post);
        String progressId = responseJson.get("result").get("progressId").asText();
        URI exportStatusUrl = new URIBuilder()
          .setScheme("https")
          .setHost(String.format("%s.qualtrics.com", DATA_CENTER))
          .setPath("/API/v3/surveys/" + SURVEY_ID + "/export-responses/" + progressId)
          .build();

        // Step 2: Wait for the export to complete
        String fileId;
        JsonNode statusJson = null;
        String progressStatus;
        progressStatus = "InProgress";
        while (!"complete".equals(progressStatus) & !"failed".equals(progressStatus)) {
            HttpGet statusGet = new HttpGet(exportStatusUrl);
            statusGet.setHeader("X-API-TOKEN", API_TOKEN);
            statusJson = sendRequest(statusGet);
            progressStatus = statusJson.get("result").get("status").asText();
            Thread.sleep(1000);
        }
        if ("Failed".equals(progressStatus)) {
            // end here
            throw new java.lang.Exception("Export Failed.");
        }

        fileId = statusJson.get("result").get("fileId").asText();

        // Step 3: Download the exported file
        URI fileUrl = new URIBuilder()
          .setScheme("https")
          .setHost(String.format("%s.qualtrics.com", DATA_CENTER))
          .setPath("/API/v3/surveys/" + SURVEY_ID + "/export-responses/" + fileId + "/file")
          .build();
        HttpGet statusGet = new HttpGet(fileUrl);
        statusGet.setHeader("X-API-TOKEN", API_TOKEN);
        HttpResponse response = httpClient.execute(statusGet);

        // Step 4: Extract exported file
        ZipInputStream zs = new ZipInputStream(response.getEntity().getContent());
        ZipEntry entry;
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"); 
        String timestamp = LocalDateTime.now().format(formatter); 
        while ((entry = zs.getNextEntry()) != null) {
            logger.log(Level.INFO, "entry.getName()={0}", entry.getName());
            //FileOutputStream out = new FileOutputStream("./" + entry.getName());
            FileOutputStream out = new FileOutputStream("./" + "Benchmarking2Metrics_"+timestamp+".csv");
            IOUtils.copy(zs, out);
            out.close();
        }
    }

    private static JsonNode sendRequest(HttpRequestBase httpRequest) throws IOException {
        HttpResponse response = httpClient.execute(httpRequest);
        String body = EntityUtils.toString(response.getEntity());
        return mapper.readTree(body);
    }
}
