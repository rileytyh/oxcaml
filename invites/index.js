// /invites/index.js
const { onDocumentCreated } = require("firebase-functions/v2/firestore");
const admin = require("firebase-admin");

admin.initializeApp();

exports.notifyInvite = onDocumentCreated("invites/{inviteId}", async (event) => {
  const snap = event.data;
  if (!snap) return;

  const inv = snap.data();
  if (!inv || !inv.toUid) return;

  // read tokens under users/{toUid}/fcmTokens/*
  const tokensSnap = await admin
    .firestore()
    .collection(`users/${inv.toUid}/fcmTokens`)
    .get();

  const tokens = tokensSnap.docs.map((d) => d.id);
  if (tokens.length === 0) return;

  const payload = {
    notification: {
      title: "Backgammon invite",
      body: "You received a match invite. Open the game to join.",
    },
    data: {
      roomId: inv.roomId || "",
      inviteId: event.params.inviteId,
    },
  };

  const resp = await admin.messaging().sendEachForMulticast({
    tokens,
    ...payload,
  });

  console.log(
    "notifyInvite sent:",
    resp.successCount,
    "success,",
    resp.failureCount,
    "failed"
  );
});
