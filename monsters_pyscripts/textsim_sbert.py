from sentence_transformers import SentenceTransformer, util
model = SentenceTransformer('all-MiniLM-L6-v2')

def get_text_sim(question1, question2):
    #Sentences are encoded by calling model.encode()
    emb1 = model.encode(question1)
    emb2 = model.encode(question2)
    cos_sim = util.cos_sim(emb1, emb2)
    return cos_sim[0][0].item()
