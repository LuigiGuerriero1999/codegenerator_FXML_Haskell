package com.example.testboy.structures;

public class GTKWidget {
    private String id; //indien in fxml expliciet met :id wordt gewerkt
    private Integer id_hash;  //interne hash value van widget

    public GTKWidget(String id, Integer id_hash) {
        this.id = id;
        this.id_hash = id_hash;
    }

    public GTKWidget() {
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Integer getId_hash() {
        return id_hash;
    }

    public void setId_hash(Integer id_hash) {
        this.id_hash = id_hash;
    }
}
