var DataSource = Backbone.Model.extend({
    idAttribute: "name",

    defaults: {
        "name": "<UNKNOWN>"
    }
});

var DataSources = Backbone.Collection.extend({
    model: DataSource,
    url: "/api/data-sources",

    parse: function(response) {
        // The API returns an array of data source names currently, so
        // we need to transform those into actual data source objects.
        return _.map(response, function(name) {
            return { "name": name };
        });
    }
});

var DataSourceView = Backbone.View.extend({
    tagName: "li",
    template: _.template(document.id("data-source-template").get("html")),

    initialize: function() {
        this.model.on("change", this.render, this);
        this.model.on("destroy", this.remove, this);
    },

    render: function() {
        this.$el.html(this.template(this.model.toJSON()));
        return this;
    }
});

var DataSourcesView = Backbone.View.extend({
    // This is the main application view, so it attaches to a previously
    // existing element.
    el: document.id("data-sources"),

    initialize: function() {
        this.list    = this.$("#data-sources-list")[0];
        this.loading = this.$("#data-sources-loading")[0];

        this.collection.on("reset", this.addAll, this);
        this.collection.on("all", this.render, this);

        // Kick off the fetch to grab all our data sources
        this.collection.fetch();
    },

    render: function() {
        // If we have any items in our collection then we hide the loading
        // dialog
        if (this.collection.length) {
            dbg.info("Collection has elements, hiding loading indicator.");
            this.loading.setStyle("display", "none");
        }

        return this;
    },

    addAll: function() {
        dbg.debug("Adding all elements from the collection");
        this.collection.each(this.addOne, this);
    },

    addOne: function(ds) {
        var view = new DataSourceView({ model: ds });
        var el   = view.render().el;
        el.inject(this.list);
    }
});
