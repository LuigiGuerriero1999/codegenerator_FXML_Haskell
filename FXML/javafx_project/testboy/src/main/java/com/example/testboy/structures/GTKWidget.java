package com.example.testboy.structures;

public class GTKWidget {
    private String id; //indien in fxml expliciet met :id wordt gewerkt
    private Integer id_hash;  //interne hash value van widget
    private String name;


    public GTKWidget(String id, Integer id_hash, String name) {
        this.id = id;
        this.id_hash = id_hash;
        this.name = name;
    }

    public GTKWidget() {
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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
