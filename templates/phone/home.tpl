{% extends "base.tpl" %}

{% block content %}

<div class="hero-unit">
    <h1>{{ m.rsc.page_home.title }}</h1>
    <p>{{ m.rsc.page_home.summary }}</p>
    {% button tag="a" class="pull-right btn btn-primary btn-large" action={redirect dispatch=`admin`} text=_"Visit Admin Interface" %}
</div>

{{ m.rsc.page_home.body }}

{% button class="btn btn-info" action={redirect dispatch=`admin_edit_rsc` id=`page_home`} text=_"Edit this page" %}

{% include "_content_list.tpl" list=m.search[{query sort='-rsc.modified' pagelen=10}] title=_"Recent content" %}
{% endblock %}
