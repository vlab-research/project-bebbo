import pandas as pd
from vlab_prepro import Preprocessor
from toolz import pipe


def pivot_data(responses):
    p = Preprocessor()
    p.keys = p.keys | set(
        [
            "surveyid",
            "shortcode",
            "survey_name",
            "version",
            "survey_created",
            "survey_start_time",
            "survey_end_time",
            "survey_duration",
            "answer_time_min",
            "answer_time_median",
            "answer_time_75",
            "answer_time_90",
            "flowid",
        ]
    )

    return pipe(
        responses,
        p.keep_final_answer,
        p.add_metadata(["seed", "creative"]),
        p.drop_users_without("creative"),
        p.drop_duplicated_users(["shortcode"]),
        p.pivot("translated_response"),
    )


def make_lookup(lng):
    return (
        lng[lng.response.notna() & lng.translated_response.notna()]
        .groupby(["shortcode", "question_ref", "response"])
        .first()
        .reset_index()[["shortcode", "question_ref", "response", "translated_response"]]
    )


def replace_and_pivot(lng):
    lookup = make_lookup(lng)
    ll = lng.drop(columns=["translated_response"]).merge(
        lookup, on=["shortcode", "question_ref", "response"], how="left"
    )
    ll["translated_response"] = ll.translated_response.combine_first(ll.response)
    return pivot_data(ll)


def read_and_format(country):
    lng = pd.read_csv(f"raw/{country}/responses-long.csv")
    out = replace_and_pivot(lng)
    out.to_csv(f"processed/{country}/responses.csv", index=False)


read_and_format("serbia")
read_and_format("bulgaria")
